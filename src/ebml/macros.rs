macro_rules! unwrap_value {
    ($field_name:ident, Option<$field_type:ty>, $field_id:literal) => {};
    ($field_name:ident, $field_type:ty, $field_id:literal, $default:expr) => {
        let $field_name = $field_name.unwrap_or($default);
    };
    ($field_name:ident, $field_type:ty, $field_id:literal) => {
        let $field_name = $crate::ebml::parse::get_required($field_name, $field_id)?;
    };
}

macro_rules! unwrap_parser {
    ($field_id:literal, Option<$field_type:ty>) => {
        $crate::ebml::macros::unwrap_parser!($field_id, $field_type)
    };
    ($field_id:literal, 0, Vec<$field_type:ty>) => {
        nom::multi::many0($crate::ebml::macros::unwrap_parser!($field_id, $field_type))
    };
    ($field_id:literal, 1, Vec<$field_type:ty>) => {
        nom::multi::many1($crate::ebml::macros::unwrap_parser!($field_id, $field_type))
    };
    ($field_id:literal, $field_type:ty) => {
        $crate::ebml::parse::ebml_element::<$field_type>($field_id)
    };
}

// This macro either returns the provided default value,
// or (if none is provided) provides the fallback default value.
macro_rules! unwrap_default {
    (, u64) => {
        0
    };
    (, u32) => {
        0
    };
    (, i64) => {
        0
    };
    (, f64) => {
        0.0
    };
    (, Date) => {
        Date(0)
    };
    (, String) => {
        String::new()
    };
    ($default:expr, $($field_type:tt)+) => {
        $default
    };
}

macro_rules! impl_ebml_default_inner {
    ($id:literal, $default:expr, $type:ty, $for_type:ty) => {
        impl $crate::ebml::serialize::EbmlDefault<$id, $type> for $for_type {
            fn default() -> $type {
                $default
            }
        }
    };
    ($id:literal, $default:expr, $type:ty) => {
        impl $crate::ebml::serialize::EbmlDefault<$id, $type> for $type {
            fn default() -> $type {
                $default
            }
        }
    };
}

macro_rules! impl_ebml_default {
    ($id:literal, $($default:expr)?, u64) => {
        $crate::ebml::macros::impl_ebml_default_inner!(
            $id,
            $crate::ebml::macros::unwrap_default!($($default)?, u64),
            u64
        );
    };
    ($id:literal, $($default:expr)?, u32) => {
        $crate::ebml::macros::impl_ebml_default_inner!(
            $id,
            $crate::ebml::macros::unwrap_default!($($default)?, u32),
            u32
        );
    };
    ($id:literal, $($default:expr)?, i64) => {
        $crate::ebml::macros::impl_ebml_default_inner!(
            $id,
            $crate::ebml::macros::unwrap_default!($($default)?, i64),
            i64
        );
    };
    ($id:literal, $($default:expr)?, f64) => {
        $crate::ebml::macros::impl_ebml_default_inner!(
            $id,
            $crate::ebml::macros::unwrap_default!($($default)?, f64),
            f64
        );
    };
    ($id:literal, $($default:expr)?, Date) => {
        $crate::ebml::macros::impl_ebml_default_inner!(
            $id,
            $crate::ebml::macros::unwrap_default!($($default)?, i64),
            i64,
            Date
        );
    };
    ($id:literal, $($default:expr)?, String) => {
        $crate::ebml::macros::impl_ebml_default_inner!(
            $id,
            $crate::ebml::macros::unwrap_default!($($default)?, String),
            String
        );
    };
    ($id:literal, $($default:expr)?, Option<$inner:tt>) => {
        $crate::ebml::macros::impl_ebml_default!($id, $($default)?, $inner);
    };
    ($id:literal, $($default:expr)?, Vec<$inner:tt>) => {
        $crate::ebml::macros::impl_ebml_default!($id, $($default)?, $inner);
    };
    ($id:literal, $($default:expr)?, $($rest:tt)+) => {};
}

/// The main macro, automagically implementing EbmlParsable and EbmlSerializable for our EBML Master Elements.
///
/// The macro requires a struct definition adhering to the following syntax:
/// ```ignore
/// # struct MyOtherMasterElement;
/// 
/// impl_ebml_master! {
///     #[derive(Debug, Clone)] // Optional
///     [0xABCD]                // Required: Element ID
///     struct MyMasterElement {
///         // One or more fields, defined like this:
///         // [ID] field_name: FieldType,
/// 
///         // Some examples:
///         [0xE1] required_uint: u64,
///         [0xE2] optional_int: Option<i64>,
///         [0xC0] sub_element: MyOtherMasterElement,
/// 
///         // If you specify a default value, the Element becomes optional.
///         // Not all types of EBML Element support default values.
///         [0xF0] optional_value: i64 = -5,
/// 
///         // For Elements that can occur more than once, Vec<T> is available.
///         // Vec<T> requires specifying a minimum amount of Elements like so:
///         [0xC1] repeating_float: Vec<f64> [0..],
///         [0xC1] repeating_int: Vec<i64> [1..],
///         // Note that only zero and one are currently supported for this.
///     }
/// }
/// ```
macro_rules! impl_ebml_master {
    (
        $(#[$outer:meta])*
        [$struct_id:literal]
        struct $name:ident$(<$lifetime:lifetime>)? {
            $([$field_id:literal] $field_name:ident: ($($field_type:tt)+) $([$lower_bound:tt..])? $(= $default:expr)?,)+
        }
    ) => {
        $(#[$outer])*
        pub struct $name$(<$lifetime>)? {
            $(pub $field_name: $($field_type)+,)+
        }

        impl<'p$(:$lifetime, $lifetime)?> $crate::ebml::EbmlParsable<'p> for $name$(<$lifetime>)? {
            // Master Elements can always have a CRC-32 Element
            fn has_crc() -> bool {
                true
            }

            fn try_parse(input: &'p [u8]) -> Result<Self, $crate::ebml::ErrorKind> {
                #[allow(unused_parens)]
                let (i, ($($field_name),*)) = $crate::permutation::matroska_permutation((
                    $($crate::ebml::macros::unwrap_parser!($field_id, $($lower_bound,)? $($field_type)+)),+
                ))(input)
                    .map_err(|e| match e {
                        nom::Err::Failure(e) | nom::Err::Error(e) => e.kind,
                        nom::Err::Incomplete(_) => $crate::ebml::ErrorKind::Nom(nom::error::ErrorKind::Complete),
                    })?;

                if !i.is_empty() {
                    log::warn!("{} unused bytes left after parsing {}", i.len(), stringify!($name));
                }

                $($crate::ebml::macros::unwrap_value!($field_name, $($field_type)+, $field_id $(, $default)?);)*

                Ok(
                    Self {
                        $($field_name),*
                    }
                )
            }
        }

        impl$(<$lifetime>)? $crate::ebml::EbmlSerializable<$struct_id, $name$(<$lifetime>)?> for $name$(<$lifetime>)? {
            fn serialize<W: std::io::Write>(
                &self,
                w: cookie_factory::WriteContext<W>,
            ) -> cookie_factory::GenResult<W> {
                let w = $crate::ebml::serialize::vid::<$struct_id, W>(w)?;

                let buf = cookie_factory::WriteContext::from(Vec::with_capacity(self.size()));

                $(
                    let buf = <$($field_type)+ as $crate::ebml::EbmlSerializable<$field_id, $($field_type)+>>::serialize(
                        &self.$field_name,
                        buf,
                    )?;
                )+

                debug_assert_eq!(self.size(), buf.position as usize);

                let w = $crate::ebml::serialize::vint(buf.position)(w)?;
                cookie_factory::combinator::slice(buf.write)(w)
            }

            fn data_size(&self) -> usize {
                let data_size = $(
                    <$($field_type)+ as $crate::ebml::EbmlSerializable<$field_id, $($field_type)+>>::size(
                        &self.$field_name
                    ) +
                )+ 0;

                $crate::ebml::serialize::vid_size::<$struct_id>() as usize+
                $crate::ebml::serialize::vint_size(data_size as u64).unwrap() as usize+
                data_size
            }
        }

        $($crate::ebml::macros::impl_ebml_default!($field_id, $($default)?, $($field_type)+);)+
    };
}

pub(crate) use impl_ebml_default;
pub(crate) use impl_ebml_default_inner;
pub(crate) use impl_ebml_master;
pub(crate) use unwrap_default;
pub(crate) use unwrap_parser;
pub(crate) use unwrap_value;
