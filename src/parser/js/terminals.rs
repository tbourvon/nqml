#[derive(Debug, PartialEq)]
pub struct NumericLiteral(pub f64);

#[derive(Debug)]
pub struct StringLiteral<'a>(pub &'a str);

pub mod parsing {

    use nom::{IResult, digit, hex_digit};
    use parser::helpers::parsing::*;
    use parser::js::terminals::*;

    named!(pub numeric_literal<&str, NumericLiteral>, conv!(NumericLiteral(do_parse!(
        take_while_s!(is_whitespace) >>
        numeric_literal: alt!(
            decimal_literal
            |
            hex_integer_literal
        ) >>
        (numeric_literal)
    ))));

    named!(decimal_literal<&str, f64>, do_parse!(
        dec_lit: recognize!(alt!(
            do_parse!(
                decimal_integer_literal >>
                do_parse!(
                    opt!(do_parse!(
                        tag_s!(".") >>
                        opt!(digit) >>
                        ()
                    )) >>
                    opt!(exponential_part) >>
                    ()
                ) >>
                ()
            )
            |
            do_parse!(
                tag_s!(".") >>
                digit >>
                opt!(exponential_part) >>
                ()
            )
        )) >>
        (dec_lit.parse::<f64>().unwrap())
    ));

    named!(decimal_integer_literal<&str, &str>, recognize!(alt!(
        do_parse!(tag_s!("0") >> ())
        |
        do_parse!(
            not!(tag_s!("0")) >>
            digit >>
            ()
        )
    )));

    named!(exponential_part<&str, &str>, recognize!(do_parse!(
        alt!(tag_s!("e") | tag_s!("E")) >>
        opt!(alt!(tag_s!("+") | tag_s!("-"))) >>
        digit >>
        ()
    )));

    named!(hex_integer_literal<&str, f64>, do_parse!(
        alt!(tag_s!("0x") | tag_s!("0X")) >>
        hex: hex_digit >>
        (i64::from_str_radix(hex, 16).unwrap() as f64)
    ));

    named!(pub string_literal<&str, StringLiteral>, do_parse!(
        delimiter: alt!(
            keyword!("\"")
            |
            keyword!("'")
        ) >>
        string: take_until_s!(delimiter) >> // TODO: escape chars
        keyword!(delimiter) >>
        (StringLiteral(string))
    ));

    named!(pub identifier<&str, &str>, do_parse!(
        take_while_s!(is_whitespace) >>
        identifier: recognize!(do_parse!(
            xid_start >>
            many0!(xid_continue) >>
            ()
        )) >>
        (identifier)
    ));

    named!(pub js_identifier<&str, &str>, alt!(
        keyword!("property")
        |
        keyword!("signal")
        |
        keyword!("readonly")
        |
        keyword!("on")
        |
        keyword!("get")
        |
        keyword!("set")
        |
        identifier
    ));

    named!(pub reserved_identifier<&str, &str>, alt!(
        keyword!("break")
        |
        keyword!("case")
        |
        keyword!("catch")
        |
        keyword!("continue")
        |
        keyword!("default")
        |
        keyword!("delete")
        |
        keyword!("do")
        |
        keyword!("else")
        |
        keyword!("false")
        |
        keyword!("finally")
        |
        keyword!("for")
        |
        keyword!("function")
        |
        keyword!("if")
        |
        keyword!("in")
        |
        keyword!("instanceof")
        |
        keyword!("new")
        |
        keyword!("null")
        |
        keyword!("return")
        |
        keyword!("switch")
        |
        keyword!("this")
        |
        keyword!("throw")
        |
        keyword!("true")
        |
        keyword!("try")
        |
        keyword!("typeof")
        |
        keyword!("var")
        |
        keyword!("void")
        |
        keyword!("while")
        |
        keyword!("const")
        |
        keyword!("debugger")
        |
        keyword!("with")
    ));

    named!(pub property_identifier<&str, &str>, alt!(
        reserved_identifier
        |
        js_identifier
    ));

    named!(pub assignment_operator<&str, &str>, alt!(
        keyword!("=")
        |
        keyword!("*=")
        |
        keyword!("/=")
        |
        keyword!("%=")
        |
        keyword!("+=")
        |
        keyword!("-=")
        |
        keyword!("<<=")
        |
        keyword!(">>>=")
        |
        keyword!(">>=")
        |
        keyword!("&=")
        |
        keyword!("^=")
        |
        keyword!("|=")
    ));

    named!(pub automatic_semicolon<&str, &str>, alt!(
        keyword!(";")
        |
        peek!(line_terminator)
        |
        peek!(keyword!("}"))
        |
        eof
    ));

    fn eof(i: &str) -> IResult<&str, &str> {
        eof!(i,)
    }

    named!(pub line_terminator<&str, &str>, alt!(
        tag_s!("\r\n")
        |
        tag_s!("\n")
        |
        tag_s!("\u{2028}")
        |
        tag_s!("\u{2029}")
    ));

}