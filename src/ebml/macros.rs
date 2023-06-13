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

macro_rules! wrap_default {
    ($default:expr) => {
        Some($default)
    };
    () => {
        None
    };
}

macro_rules! impl_ebml_master {
    (
        $(#[$outer:meta])*
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

        impl$(<$lifetime>)? $crate::ebml::EbmlSerializable for $name$(<$lifetime>)? {
            fn serialize<const ID: u32, W: std::io::Write>(
                &self,
                w: cookie_factory::WriteContext<W>,
                default: std::option::Option<Self>
            ) -> cookie_factory::GenResult<W> {
                assert!(default.is_none(), "Default values are not supported for EBML Master Elements");

                let w = $crate::ebml::serialize::vid::<ID, W>(w)?;

                // TODO: Parametrize starting capacity
                let buf = cookie_factory::WriteContext::from(std::vec::Vec::with_capacity(64));

                $(
                    let buf = self.$field_name.serialize::<$field_id, _>(
                        buf,
                        $crate::ebml::macros::wrap_default!($($default)?)
                    )?;
                )+

                let w = $crate::ebml::serialize::vint(buf.position as u64)(w)?;
                cookie_factory::combinator::slice(buf.write)(w)
            }
        }
    };
}

pub(crate) use impl_ebml_master;
pub(crate) use unwrap_parser;
pub(crate) use unwrap_value;
pub(crate) use wrap_default;
