#[derive(Debug)]
pub struct NumericLiteral(pub f64);
#[derive(Debug)]
pub struct StringLiteral<'a>(pub &'a str);

pub mod parsing {

    use nom::{IResult, double_s, digit};
    use parser::helpers::parsing::*;
    use parser::js::terminals::*;

    named!(pub numeric_literal<&str, NumericLiteral>, conv!(NumericLiteral(do_parse!(
        take_while_s!(is_whitespace) >>
        numeric_literal: alt!(
            double_s
            |
            do_parse!(
                int: digit >>
                (int.parse::<f64>().unwrap())
            )
        ) >>
        (numeric_literal)
    ))));

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