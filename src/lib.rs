#![feature(unicode)]
#![cfg_attr(feature="clippy", feature(plugin))]
#![cfg_attr(feature="clippy", plugin(clippy))]

#[macro_use]
extern crate nom;

pub mod parser;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        
    }
}
