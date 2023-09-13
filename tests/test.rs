use typeof_literal::typeof_literal;

trait Builder<T> {
    fn build() -> T;
}

struct SimpleBuilder;

macro_rules! define_builder {
    ($value:expr) => {
        impl Builder<typeof_literal! {$value}> for SimpleBuilder {
            fn build() -> typeof_literal! {$value} {
                $value
            }
        }
    };
}

define_builder! {10i64}
define_builder! {12}
define_builder! {5.5}
define_builder! {"HELLO"}
define_builder! {b"some bytes"}
define_builder! {(10, "string")}
define_builder! {[1, 2, 3, 4, 5]}
define_builder! {[0u8; 5]}
