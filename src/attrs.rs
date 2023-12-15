// SPDX-License-Identifier: MIT OR Apache-2.0

#[cfg(not(feature = "std"))]
use alloc::{
    string::{String, ToString},
    vec::Vec,
};
use core::ops::Range;

pub use fontdb::{Family, Stretch, Style, Weight};
use rangemap::RangeMap;

/// Text color
#[derive(Clone, Copy, Debug, PartialOrd, Ord, Eq, Hash, PartialEq)]
pub struct Color(pub u32);

impl Color {
    /// Create new color with red, green, and blue components
    #[inline]
    pub const fn rgb(r: u8, g: u8, b: u8) -> Self {
        Self::rgba(r, g, b, 0xFF)
    }

    /// Create new color with red, green, blue, and alpha components
    #[inline]
    pub const fn rgba(r: u8, g: u8, b: u8, a: u8) -> Self {
        Self(((a as u32) << 24) | ((r as u32) << 16) | ((g as u32) << 8) | (b as u32))
    }

    /// Get a tuple over all of the attributes, in `(r, g, b, a)` order.
    #[inline]
    pub fn as_rgba_tuple(self) -> (u8, u8, u8, u8) {
        (self.r(), self.g(), self.b(), self.a())
    }

    /// Get an array over all of the components, in `[r, g, b, a]` order.
    #[inline]
    pub fn as_rgba(self) -> [u8; 4] {
        [self.r(), self.g(), self.b(), self.a()]
    }

    /// Get the red component
    #[inline]
    pub fn r(&self) -> u8 {
        ((self.0 & 0x00_FF_00_00) >> 16) as u8
    }

    /// Get the green component
    #[inline]
    pub fn g(&self) -> u8 {
        ((self.0 & 0x00_00_FF_00) >> 8) as u8
    }

    /// Get the blue component
    #[inline]
    pub fn b(&self) -> u8 {
        (self.0 & 0x00_00_00_FF) as u8
    }

    /// Get the alpha component
    #[inline]
    pub fn a(&self) -> u8 {
        ((self.0 & 0xFF_00_00_00) >> 24) as u8
    }
}

/// An owned version of [`Family`]
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum FamilyOwned {
    Name(String),
    Serif,
    SansSerif,
    Cursive,
    Fantasy,
    Monospace,
}

impl FamilyOwned {
    pub fn new(family: Family) -> Self {
        match family {
            Family::Name(name) => FamilyOwned::Name(name.to_string()),
            Family::Serif => FamilyOwned::Serif,
            Family::SansSerif => FamilyOwned::SansSerif,
            Family::Cursive => FamilyOwned::Cursive,
            Family::Fantasy => FamilyOwned::Fantasy,
            Family::Monospace => FamilyOwned::Monospace,
        }
    }

    pub fn as_family(&self) -> Family {
        match self {
            FamilyOwned::Name(name) => Family::Name(name),
            FamilyOwned::Serif => Family::Serif,
            FamilyOwned::SansSerif => Family::SansSerif,
            FamilyOwned::Cursive => Family::Cursive,
            FamilyOwned::Fantasy => Family::Fantasy,
            FamilyOwned::Monospace => Family::Monospace,
        }
    }
}

/// Determines the line height and strategy.
/// The actual height of a line will be determined by the largest logical line height in a line.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum LineHeight {
    /// Represents a line height that is proportional to the font size.
    Proportional(f32),
    /// Represents an absolute line height, independent of the font size.
    Absolute(f32),
}

impl core::hash::Hash for LineHeight {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        match self {
            LineHeight::Proportional(height) => {
                state.write_u8(1);
                height.to_bits().hash(state);
            }
            LineHeight::Absolute(height) => {
                state.write_u8(2);
                height.to_bits().hash(state);
            }
        }
    }
}

impl LineHeight {
    pub fn height(&self, font_size: f32) -> f32 {
        match self {
            LineHeight::Proportional(height) => *height * font_size,
            LineHeight::Absolute(height) => *height,
        }
    }
}

impl Default for LineHeight {
    fn default() -> Self {
        Self::Proportional(1.2)
    }
}

/// Text attributes
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Attrs<'a> {
    //TODO: should this be an option?
    // TODO: extract
    pub color_opt: Option<Color>,
    pub family: Family<'a>,
    pub stretch: Stretch,
    pub style: Style,
    pub weight: Weight,
    // TODO: extract
    pub metadata: usize,
    // TODO: extract
    pub font_size: f32,
    // TODO: extract
    pub line_height: LineHeight,
}

impl<'a> Attrs<'a> {
    /// Create a new set of attributes with sane defaults
    ///
    /// This defaults to a regular Sans-Serif font.
    pub fn new() -> Self {
        Self {
            color_opt: None,
            family: Family::SansSerif,
            stretch: Stretch::Normal,
            style: Style::Normal,
            weight: Weight::NORMAL,
            font_size: 16.0,
            line_height: LineHeight::Proportional(1.2),
            metadata: 0,
        }
    }

    /// Set [Color]
    pub fn color(mut self, color: Color) -> Self {
        self.color_opt = Some(color);
        self
    }

    /// Set font size
    ///
    /// # Panics
    ///
    /// Will panic if font size is zero.
    pub fn font_size(mut self, size: f32) -> Self {
        assert_ne!(size, 0.0, "font size cannot be 0");
        self.font_size = size;
        self
    }

    /// Set line height
    ///
    /// # Panics
    ///
    /// Will panic if line height is zero.
    pub fn line_height(mut self, line_height: LineHeight) -> Self {
        let inner = match line_height {
            LineHeight::Absolute(inner) | LineHeight::Proportional(inner) => inner,
        };
        assert_ne!(inner, 0.0, "line height cannot be 0");
        self.line_height = line_height;
        self
    }

    /// Set [Family]
    pub fn family(mut self, family: Family<'a>) -> Self {
        self.family = family;
        self
    }

    /// Set [Stretch]
    pub fn stretch(mut self, stretch: Stretch) -> Self {
        self.stretch = stretch;
        self
    }

    /// Set [Style]
    pub fn style(mut self, style: Style) -> Self {
        self.style = style;
        self
    }

    /// Set [Weight]
    pub fn weight(mut self, weight: Weight) -> Self {
        self.weight = weight;
        self
    }

    /// Set metadata
    pub fn metadata(mut self, metadata: usize) -> Self {
        self.metadata = metadata;
        self
    }

    /// Check if font matches
    pub fn matches(&self, face: &fontdb::FaceInfo) -> bool {
        //TODO: smarter way of including emoji
        face.post_script_name.contains("Emoji")
            || (face.style == self.style
                && face.weight == self.weight
                && face.stretch == self.stretch)
    }

    /// Check if this set of attributes can be shaped with another
    pub fn compatible(&self, other: &Self) -> bool {
        self.family == other.family
            && self.stretch == other.stretch
            && self.style == other.style
            && self.weight == other.weight
    }

    /// Scale the font size and line height
    ///
    /// # Panics
    ///
    /// Will panic if scale is zero.
    pub fn scale(mut self, scale: f32) -> Self {
        assert_ne!(scale, 0.0, "scale cannot be 0");
        self.font_size = self.font_size * scale;
        if let LineHeight::Absolute(height) = self.line_height {
            self.line_height = LineHeight::Absolute(height * scale);
        }
        self
    }
}

impl<'a> Eq for Attrs<'a> {}

impl<'a> core::hash::Hash for Attrs<'a> {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        self.color_opt.hash(state);
        self.family.hash(state);
        self.stretch.hash(state);
        self.style.hash(state);
        self.weight.hash(state);
        self.metadata.hash(state);
        self.font_size.to_bits().hash(state);
        self.line_height.hash(state);
    }
}

/// An owned version of [`Attrs`]
#[derive(Clone, Debug, PartialEq)]
pub struct AttrsOwned {
    //TODO: should this be an option?
    // TODO: extract
    pub color_opt: Option<Color>,
    pub family_owned: FamilyOwned,
    pub stretch: Stretch,
    pub style: Style,
    pub weight: Weight,
    // TODO: extract
    pub metadata: usize,
    // TODO: extract
    pub font_size: f32,
    // TODO: extract
    pub line_height: LineHeight,
}

impl AttrsOwned {
    pub fn new(attrs: Attrs) -> Self {
        Self {
            color_opt: attrs.color_opt,
            family_owned: FamilyOwned::new(attrs.family),
            stretch: attrs.stretch,
            style: attrs.style,
            weight: attrs.weight,
            metadata: attrs.metadata,
            font_size: attrs.font_size,
            line_height: attrs.line_height,
        }
    }

    pub fn as_attrs(&self) -> Attrs {
        Attrs {
            color_opt: self.color_opt,
            family: self.family_owned.as_family(),
            stretch: self.stretch,
            style: self.style,
            weight: self.weight,
            metadata: self.metadata,
            font_size: self.font_size,
            line_height: self.line_height,
        }
    }
}

impl Eq for AttrsOwned {}

impl core::hash::Hash for AttrsOwned {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        self.color_opt.hash(state);
        self.family_owned.hash(state);
        self.stretch.hash(state);
        self.style.hash(state);
        self.weight.hash(state);
        self.metadata.hash(state);
        self.font_size.to_bits().hash(state);
        self.line_height.hash(state);
    }
}

/// List of text attributes to apply to a line
//TODO: have this clean up the spans when changes are made
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct AttrsList {
    defaults: AttrsOwned,
    spans: RangeMap<usize, AttrsOwned>,
}

impl AttrsList {
    /// Create a new attributes list with a set of default [Attrs]
    pub fn new(defaults: Attrs) -> Self {
        Self {
            defaults: AttrsOwned::new(defaults),
            spans: RangeMap::new(),
        }
    }

    /// Get the default [Attrs]
    pub fn defaults(&self) -> Attrs {
        self.defaults.as_attrs()
    }

    /// Get the current attribute spans
    pub fn spans(&self) -> Vec<(&Range<usize>, &AttrsOwned)> {
        self.spans.iter().collect()
    }

    /// Clear the current attribute spans
    pub fn clear_spans(&mut self) {
        self.spans.clear();
    }

    /// Add an attribute span, removes any previous matching parts of spans
    pub fn add_span(&mut self, range: Range<usize>, attrs: Attrs) {
        //do not support 1..1 even if by accident.
        if range.start == range.end {
            return;
        }

        self.spans.insert(range, AttrsOwned::new(attrs));
    }

    /// Get the attribute span for an index
    ///
    /// This returns a span that contains the index
    pub fn get_span(&self, index: usize) -> Attrs {
        self.spans
            .get(&index)
            .map(|v| v.as_attrs())
            .unwrap_or(self.defaults.as_attrs())
    }

    /// Split attributes list at an offset
    pub fn split_off(&mut self, index: usize) -> Self {
        let mut new = Self::new(self.defaults.as_attrs());
        let mut removes = Vec::new();

        //get the keys we need to remove or fix.
        for span in self.spans.iter() {
            if span.0.end <= index {
                continue;
            } else if span.0.start >= index {
                removes.push((span.0.clone(), false));
            } else {
                removes.push((span.0.clone(), true));
            }
        }

        for (key, resize) in removes {
            let (range, attrs) = self
                .spans
                .get_key_value(&key.start)
                .map(|v| (v.0.clone(), v.1.clone()))
                .expect("attrs span not found");
            self.spans.remove(key);

            if resize {
                new.spans.insert(0..range.end - index, attrs.clone());
                self.spans.insert(range.start..index, attrs);
            } else {
                new.spans
                    .insert(range.start - index..range.end - index, attrs);
            }
        }
        new
    }
}
