// SPDX-License-Identifier: MIT OR Apache-2.0

use alloc::boxed::Box;
use alloc::sync::Arc;
use alloc::vec::Vec;
use core::{mem, ops::Range};
use fontdb::Family;
// pub because Script is part of the interface of the Fallback trait
pub use unicode_script::Script;

use crate::{Font, FontMatchKey, FontSystem, ShapeBuffer};

#[cfg(not(any(all(unix, not(target_os = "android")), target_os = "windows")))]
#[path = "other.rs"]
mod platform;

#[cfg(target_os = "macos")]
#[path = "macos.rs"]
mod platform;

#[cfg(all(unix, not(any(target_os = "android", target_os = "macos"))))]
#[path = "unix.rs"]
mod platform;

#[cfg(target_os = "windows")]
#[path = "windows.rs"]
mod platform;

/// The `Fallback` trait allows for configurable font fallback lists to be set during construction of the [`FontSystem`].
///
/// A custom fallback list can be added via the [`FontSystem::new_with_locale_and_db_and_fallback`] constructor.
///
/// A default implementation is provided by the [`PlatformFallback`] struct, which encapsulates the target platform's pre-configured fallback lists.
///
/// ```rust
/// use cosmic_text::{fontdb, Fallback, FontSystem, Script};
///
/// struct MyFallback;
///
/// impl Fallback for MyFallback {
///     fn common_fallback(&self) -> &[&'static str] {
///         &[
///             "Segoe UI",
///             "Segoe UI Emoji",
///             "Segoe UI Symbol",
///             "Segoe UI Historic",
///         ]
///     }
///
///     fn forbidden_fallback(&self) -> &[&'static str] {
///         &[]
///     }
///
///     fn script_fallback(&self, script: Script, locale: &str) -> &[&'static str] {
///         match script {
///             Script::Adlam => &["Ebrima"],
///             Script::Bengali => &["Nirmala UI"],
///             Script::Canadian_Aboriginal => &["Gadugi"],
///             // ...
///             _ => &[],
///        }
///     }
/// }
///
/// let locale = "en-US".to_string();
/// let db = fontdb::Database::new();
/// let font_system = FontSystem::new_with_locale_and_db_and_fallback(locale, db, MyFallback);
/// ```
pub trait Fallback: Send + Sync {
    /// Fallbacks to use after any script specific fallbacks
    fn common_fallback(&self) -> &[&'static str];

    /// Fallbacks to never use
    fn forbidden_fallback(&self) -> &[&'static str];

    /// Fallbacks to use per script
    fn script_fallback(&self, script: Script, locale: &str) -> &[&'static str];
}

#[derive(Debug, Default)]
pub struct Fallbacks {
    lists: Box<[&'static str]>,
    common_fallback_range_end: usize,
    forbidden_fallback_range_end: usize,
    script_fallback_ranges: Box<[Option<Range<usize>>]>,
}

impl Fallbacks {
    pub(crate) fn new(fallbacks: impl Fallback, locale: &str) -> Self {
        let common_fallback = fallbacks.common_fallback();

        let forbidden_fallback = fallbacks.forbidden_fallback();

        let mut lists = Vec::with_capacity(common_fallback.len() + forbidden_fallback.len());

        lists.extend_from_slice(common_fallback);
        let common_fallback_range_end = lists.len();

        lists.extend_from_slice(forbidden_fallback);
        let forbidden_fallback_range_end = lists.len();

        debug_assert!(common_fallback_range_end <= forbidden_fallback_range_end);

        let mut index = forbidden_fallback_range_end;
        let mut new_range = |lists: &Vec<&str>| {
            let old_index = index;
            index = lists.len();
            old_index..index
        };

        // unicode_script::Script is repr(u8) and has a maximum of 256 variants
        let mut script_fallback_ranges = vec![None; 256];
        for i in 0..=255 {
            let script = script_from_integer(i);
            let script_fallback = fallbacks.script_fallback(script, locale);
            lists.extend_from_slice(script_fallback);
            let script_fallback_range = new_range(&lists);
            script_fallback_ranges[script as usize] = Some(script_fallback_range);
        }

        Self {
            lists: lists.into_boxed_slice(),
            common_fallback_range_end,
            forbidden_fallback_range_end,
            script_fallback_ranges: script_fallback_ranges.into_boxed_slice(),
        }
    }

    #[inline]
    pub(crate) fn common_fallback(&self) -> &[&'static str] {
        &self.lists[0..self.common_fallback_range_end]
    }

    #[inline]
    pub(crate) fn forbidden_fallback(&self) -> &[&'static str] {
        &self.lists[self.common_fallback_range_end..self.forbidden_fallback_range_end]
    }

    #[inline]
    pub(crate) fn script_fallback(&self, script: Script) -> &[&'static str] {
        self.script_fallback_ranges
            .get(script as usize)
            .and_then(|x| x.as_ref())
            .map_or(&[], |range| &self.lists[range.clone()])
    }
}

impl Fallback for Fallbacks {
    fn common_fallback(&self) -> &[&'static str] {
        self.common_fallback()
    }

    fn forbidden_fallback(&self) -> &[&'static str] {
        self.forbidden_fallback()
    }

    fn script_fallback(&self, script: Script, _locale: &str) -> &[&'static str] {
        self.script_fallback(script)
    }
}

pub use platform::PlatformFallback;

#[cfg(not(feature = "warn_on_missing_glyphs"))]
use log::debug as missing_warn;
#[cfg(feature = "warn_on_missing_glyphs")]
use log::warn as missing_warn;

// Match on lowest font_weight_diff, then script_non_matches, then font_weight
// Default font gets None for both `weight_offset` and `script_non_matches`, and thus, it is
// always the first to be popped from the set.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct MonospaceFallbackInfo {
    font_weight_diff: Option<u16>,
    codepoint_non_matches: Option<usize>,
    font_weight: u16,
    id: fontdb::ID,
}

pub struct FontFallbackIter<'a> {
    font_system: &'a mut FontSystem,
    font_match_keys: &'a [FontMatchKey],
    default_families: &'a [&'a Family<'a>],
    default_i: usize,
    scripts: &'a [Script],
    word: &'a str,
    script_i: (usize, usize),
    common_i: usize,
    other_i: usize,
    end: bool,
}

impl<'a> FontFallbackIter<'a> {
    pub fn new(
        font_system: &'a mut FontSystem,
        font_match_keys: &'a [FontMatchKey],
        default_families: &'a [&'a Family<'a>],
        scripts: &'a [Script],
        word: &'a str,
    ) -> Self {
        font_system.monospace_fallbacks_buffer.clear();
        Self {
            font_system,
            font_match_keys,
            default_families,
            default_i: 0,
            scripts,
            word,
            script_i: (0, 0),
            common_i: 0,
            other_i: 0,
            end: false,
        }
    }

    pub fn check_missing(&mut self, word: &str) {
        if self.end {
            missing_warn!(
                "Failed to find any fallback for {:?} locale '{}': '{}'",
                self.scripts,
                self.font_system.locale(),
                word
            );
        } else if self.other_i > 0 {
            missing_warn!(
                "Failed to find preset fallback for {:?} locale '{}', used '{}': '{}'",
                self.scripts,
                self.font_system.locale(),
                self.face_name(self.font_match_keys[self.other_i - 1].id),
                word
            );
        } else if !self.scripts.is_empty() && self.common_i > 0 {
            let family = self.font_system.fallbacks.common_fallback()[self.common_i - 1];
            missing_warn!(
                "Failed to find script fallback for {:?} locale '{}', used '{}': '{}'",
                self.scripts,
                self.font_system.locale(),
                family,
                word
            );
        }
    }

    pub fn face_name(&self, id: fontdb::ID) -> &str {
        if let Some(face) = self.font_system.db().face(id) {
            if let Some((name, _)) = face.families.first() {
                name
            } else {
                &face.post_script_name
            }
        } else {
            "invalid font id"
        }
    }

    pub fn shape_caches(&mut self) -> &mut ShapeBuffer {
        &mut self.font_system.shape_buffer
    }

    fn face_contains_family(&self, id: fontdb::ID, family_name: &str) -> bool {
        if let Some(face) = self.font_system.db().face(id) {
            face.families.iter().any(|(name, _)| name == family_name)
        } else {
            false
        }
    }

    fn default_font_match_key(&self) -> Option<&FontMatchKey> {
        let default_family = self.default_families[self.default_i - 1];
        let default_family_name = self.font_system.db().family_name(default_family);

        self.font_match_keys
            .iter()
            .filter(|m_key| m_key.font_weight_diff == 0)
            .find(|m_key| self.face_contains_family(m_key.id, default_family_name))
    }

    fn next_item(&mut self, fallbacks: &Fallbacks) -> Option<<Self as Iterator>::Item> {
        if let Some(fallback_info) = self.font_system.monospace_fallbacks_buffer.pop_first() {
            if let Some(font) = self.font_system.get_font(fallback_info.id) {
                return Some(font);
            }
        }

        let font_match_keys_iter = |is_mono| {
            self.font_match_keys
                .iter()
                .filter(move |m_key| m_key.font_weight_diff == 0 || is_mono)
        };

        'DEF_FAM: while self.default_i < self.default_families.len() {
            self.default_i += 1;
            let is_mono = self.default_families[self.default_i - 1] == &Family::Monospace;
            let default_font_match_key = self.default_font_match_key().cloned();
            let word_chars_count = self.word.chars().count();

            macro_rules! mk_mono_fallback_info {
                ($m_key:expr) => {{
                    let supported_cp_count_opt = self
                        .font_system
                        .get_font_supported_codepoints_in_word($m_key.id, self.word);

                    supported_cp_count_opt.map(|supported_cp_count| {
                        let codepoint_non_matches = word_chars_count - supported_cp_count;

                        MonospaceFallbackInfo {
                            font_weight_diff: Some($m_key.font_weight_diff),
                            codepoint_non_matches: Some(codepoint_non_matches),
                            font_weight: $m_key.font_weight,
                            id: $m_key.id,
                        }
                    })
                }};
            }

            match (is_mono, default_font_match_key.as_ref()) {
                (false, None) => break 'DEF_FAM,
                (false, Some(m_key)) => {
                    if let Some(font) = self.font_system.get_font(m_key.id) {
                        return Some(font);
                    } else {
                        break 'DEF_FAM;
                    }
                }
                (true, None) => (),
                (true, Some(m_key)) => {
                    // Default Monospace font
                    if let Some(mut fallback_info) = mk_mono_fallback_info!(m_key) {
                        fallback_info.font_weight_diff = None;

                        // Return early if default Monospace font supports all word codepoints.
                        // Otherewise, add to fallbacks set
                        if fallback_info.codepoint_non_matches == Some(0) {
                            if let Some(font) = self.font_system.get_font(m_key.id) {
                                return Some(font);
                            }
                        } else {
                            assert!(self
                                .font_system
                                .monospace_fallbacks_buffer
                                .insert(fallback_info));
                        }
                    }
                }
            };

            let mono_ids_for_scripts = if is_mono && !self.scripts.is_empty() {
                let scripts = self.scripts.iter().filter_map(|script| {
                    let script_as_lower = script.short_name().to_lowercase();
                    <[u8; 4]>::try_from(script_as_lower.as_bytes()).ok()
                });
                self.font_system.get_monospace_ids_for_scripts(scripts)
            } else {
                Vec::new()
            };

            for m_key in font_match_keys_iter(is_mono) {
                if Some(m_key.id) != default_font_match_key.as_ref().map(|m_key| m_key.id) {
                    let is_mono_id = if mono_ids_for_scripts.is_empty() {
                        self.font_system.is_monospace(m_key.id)
                    } else {
                        mono_ids_for_scripts.binary_search(&m_key.id).is_ok()
                    };

                    if is_mono_id {
                        let supported_cp_count_opt = self
                            .font_system
                            .get_font_supported_codepoints_in_word(m_key.id, self.word);
                        if let Some(supported_cp_count) = supported_cp_count_opt {
                            let codepoint_non_matches =
                                self.word.chars().count() - supported_cp_count;

                            let fallback_info = MonospaceFallbackInfo {
                                font_weight_diff: Some(m_key.font_weight_diff),
                                codepoint_non_matches: Some(codepoint_non_matches),
                                font_weight: m_key.font_weight,
                                id: m_key.id,
                            };
                            assert!(self
                                .font_system
                                .monospace_fallbacks_buffer
                                .insert(fallback_info));
                        }
                    }
                }
            }
            // If default family is Monospace fallback to first monospaced font
            if let Some(fallback_info) = self.font_system.monospace_fallbacks_buffer.pop_first() {
                if let Some(font) = self.font_system.get_font(fallback_info.id) {
                    return Some(font);
                }
            }
        }

        while self.script_i.0 < self.scripts.len() {
            let script = self.scripts[self.script_i.0];

            let script_families = fallbacks.script_fallback(script);

            while self.script_i.1 < script_families.len() {
                let script_family = script_families[self.script_i.1];
                self.script_i.1 += 1;
                for m_key in font_match_keys_iter(false) {
                    if self.face_contains_family(m_key.id, script_family) {
                        if let Some(font) = self.font_system.get_font(m_key.id) {
                            return Some(font);
                        }
                    }
                }
                log::debug!(
                    "failed to find family '{}' for script {:?} and locale '{}'",
                    script_family,
                    script,
                    self.font_system.locale(),
                );
            }

            self.script_i.0 += 1;
            self.script_i.1 = 0;
        }

        let common_families = fallbacks.common_fallback();
        while self.common_i < common_families.len() {
            let common_family = common_families[self.common_i];
            self.common_i += 1;
            for m_key in font_match_keys_iter(false) {
                if self.face_contains_family(m_key.id, common_family) {
                    if let Some(font) = self.font_system.get_font(m_key.id) {
                        return Some(font);
                    }
                }
            }
            log::debug!("failed to find family '{}'", common_family);
        }

        //TODO: do we need to do this?
        //TODO: do not evaluate fonts more than once!
        let forbidden_families = fallbacks.forbidden_fallback();
        while self.other_i < self.font_match_keys.len() {
            let id = self.font_match_keys[self.other_i].id;
            self.other_i += 1;
            if forbidden_families
                .iter()
                .all(|family_name| !self.face_contains_family(id, family_name))
            {
                if let Some(font) = self.font_system.get_font(id) {
                    return Some(font);
                }
            }
        }

        self.end = true;
        None
    }
}

impl Iterator for FontFallbackIter<'_> {
    type Item = Arc<Font>;
    fn next(&mut self) -> Option<Self::Item> {
        let mut fallbacks = mem::take(&mut self.font_system.fallbacks);
        let item = self.next_item(&fallbacks);
        mem::swap(&mut fallbacks, &mut self.font_system.fallbacks);
        item
    }
}

/// Adapted from definition of [`unicode_script::Script`]
#[inline]
fn script_from_integer(value: u8) -> Script {
    match value {
        0xFF => Script::Unknown,
        0xFE => Script::Common,
        0xFD => Script::Inherited,
        0 => Script::Adlam,
        1 => Script::Caucasian_Albanian,
        2 => Script::Ahom,
        3 => Script::Arabic,
        4 => Script::Imperial_Aramaic,
        5 => Script::Armenian,
        6 => Script::Avestan,
        7 => Script::Balinese,
        8 => Script::Bamum,
        9 => Script::Bassa_Vah,
        10 => Script::Batak,
        11 => Script::Bengali,
        12 => Script::Bhaiksuki,
        13 => Script::Bopomofo,
        14 => Script::Brahmi,
        15 => Script::Braille,
        16 => Script::Buginese,
        17 => Script::Buhid,
        18 => Script::Chakma,
        19 => Script::Canadian_Aboriginal,
        20 => Script::Carian,
        21 => Script::Cham,
        22 => Script::Cherokee,
        23 => Script::Chorasmian,
        24 => Script::Coptic,
        25 => Script::Cypro_Minoan,
        26 => Script::Cypriot,
        27 => Script::Cyrillic,
        28 => Script::Devanagari,
        29 => Script::Dives_Akuru,
        30 => Script::Dogra,
        31 => Script::Deseret,
        32 => Script::Duployan,
        33 => Script::Egyptian_Hieroglyphs,
        34 => Script::Elbasan,
        35 => Script::Elymaic,
        36 => Script::Ethiopic,
        37 => Script::Garay,
        38 => Script::Georgian,
        39 => Script::Glagolitic,
        40 => Script::Gunjala_Gondi,
        41 => Script::Masaram_Gondi,
        42 => Script::Gothic,
        43 => Script::Grantha,
        44 => Script::Greek,
        45 => Script::Gujarati,
        46 => Script::Gurung_Khema,
        47 => Script::Gurmukhi,
        48 => Script::Hangul,
        49 => Script::Han,
        50 => Script::Hanunoo,
        51 => Script::Hatran,
        52 => Script::Hebrew,
        53 => Script::Hiragana,
        54 => Script::Anatolian_Hieroglyphs,
        55 => Script::Pahawh_Hmong,
        56 => Script::Nyiakeng_Puachue_Hmong,
        57 => Script::Old_Hungarian,
        58 => Script::Old_Italic,
        59 => Script::Javanese,
        60 => Script::Kayah_Li,
        61 => Script::Katakana,
        62 => Script::Kawi,
        63 => Script::Kharoshthi,
        64 => Script::Khmer,
        65 => Script::Khojki,
        66 => Script::Khitan_Small_Script,
        67 => Script::Kannada,
        68 => Script::Kirat_Rai,
        69 => Script::Kaithi,
        70 => Script::Tai_Tham,
        71 => Script::Lao,
        72 => Script::Latin,
        73 => Script::Lepcha,
        74 => Script::Limbu,
        75 => Script::Linear_A,
        76 => Script::Linear_B,
        77 => Script::Lisu,
        78 => Script::Lycian,
        79 => Script::Lydian,
        80 => Script::Mahajani,
        81 => Script::Makasar,
        82 => Script::Mandaic,
        83 => Script::Manichaean,
        84 => Script::Marchen,
        85 => Script::Medefaidrin,
        86 => Script::Mende_Kikakui,
        87 => Script::Meroitic_Cursive,
        88 => Script::Meroitic_Hieroglyphs,
        89 => Script::Malayalam,
        90 => Script::Modi,
        91 => Script::Mongolian,
        92 => Script::Mro,
        93 => Script::Meetei_Mayek,
        94 => Script::Multani,
        95 => Script::Myanmar,
        96 => Script::Nag_Mundari,
        97 => Script::Nandinagari,
        98 => Script::Old_North_Arabian,
        99 => Script::Nabataean,
        100 => Script::Newa,
        101 => Script::Nko,
        102 => Script::Nushu,
        103 => Script::Ogham,
        104 => Script::Ol_Chiki,
        105 => Script::Ol_Onal,
        106 => Script::Old_Turkic,
        107 => Script::Oriya,
        108 => Script::Osage,
        109 => Script::Osmanya,
        110 => Script::Old_Uyghur,
        111 => Script::Palmyrene,
        112 => Script::Pau_Cin_Hau,
        113 => Script::Old_Permic,
        114 => Script::Phags_Pa,
        115 => Script::Inscriptional_Pahlavi,
        116 => Script::Psalter_Pahlavi,
        117 => Script::Phoenician,
        118 => Script::Miao,
        119 => Script::Inscriptional_Parthian,
        120 => Script::Rejang,
        121 => Script::Hanifi_Rohingya,
        122 => Script::Runic,
        123 => Script::Samaritan,
        124 => Script::Old_South_Arabian,
        125 => Script::Saurashtra,
        126 => Script::SignWriting,
        127 => Script::Shavian,
        128 => Script::Sharada,
        129 => Script::Siddham,
        130 => Script::Khudawadi,
        131 => Script::Sinhala,
        132 => Script::Sogdian,
        133 => Script::Old_Sogdian,
        134 => Script::Sora_Sompeng,
        135 => Script::Soyombo,
        136 => Script::Sundanese,
        137 => Script::Sunuwar,
        138 => Script::Syloti_Nagri,
        139 => Script::Syriac,
        140 => Script::Tagbanwa,
        141 => Script::Takri,
        142 => Script::Tai_Le,
        143 => Script::New_Tai_Lue,
        144 => Script::Tamil,
        145 => Script::Tangut,
        146 => Script::Tai_Viet,
        147 => Script::Telugu,
        148 => Script::Tifinagh,
        149 => Script::Tagalog,
        150 => Script::Thaana,
        151 => Script::Thai,
        152 => Script::Tibetan,
        153 => Script::Tirhuta,
        154 => Script::Tangsa,
        155 => Script::Todhri,
        156 => Script::Toto,
        157 => Script::Tulu_Tigalari,
        158 => Script::Ugaritic,
        159 => Script::Vai,
        160 => Script::Vithkuqi,
        161 => Script::Warang_Citi,
        162 => Script::Wancho,
        163 => Script::Old_Persian,
        164 => Script::Cuneiform,
        165 => Script::Yezidi,
        166 => Script::Yi,
        167 => Script::Zanabazar_Square,
        _ => Script::Unknown,
    }
}

#[test]
fn round_trip_script_integer_conversions() {
    for i in 0..168 {
        let script = script_from_integer(i);
        assert!(script as u8 == i, "known scripts");
    }
    for i in 168..0xFD {
        let script = script_from_integer(i);
        assert!(
            script == Script::Unknown && script as u8 == 0xFF,
            "unallocated integers for scripts"
        );
    }
    for i in 0xFD..=0xFF {
        let script = script_from_integer(i);
        assert!(script as u8 == i, "special values");
    }
}
