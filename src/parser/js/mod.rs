pub mod expressions;
pub mod statements;
pub mod terminals;

use std;
use parser::js::statements::*;

#[derive(Debug)]
pub struct Program<'a>(pub Option<SourceElements<'a>>);

#[derive(Debug, PartialEq)]
pub struct SourceElements<'a>(pub std::vec::Vec<SourceElement<'a>>);

#[derive(Debug, PartialEq)]
pub enum SourceElement<'a> {
    Statement(Statement<'a>),
    FunctionDeclaration(FunctionDeclaration<'a>),
}

#[derive(Debug, PartialEq)]
pub struct FunctionDeclaration<'a> {
    pub name: &'a str,
    pub formals: Option<FormalParameterList<'a>>,
    pub body: Option<FunctionBody<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct FormalParameterList<'a>(pub std::vec::Vec<FormalParameter<'a>>);
#[derive(Debug, PartialEq)]
pub struct FormalParameter<'a>(pub &'a str);

#[derive(Debug, PartialEq)]
pub struct FunctionBody<'a>(pub SourceElements<'a>);

pub mod parsing {

    use parser::helpers::parsing::*;
    use parser::js::*;
    use parser::js::statements::parsing::*;
    use parser::js::terminals::parsing::*;

    named!(pub program<&str, Program>, conv!(Program(opt!(source_elements))));

    named!(source_elements<&str, SourceElements>, conv!(SourceElements(many1!(source_element))));

    named!(pub source_element<&str, SourceElement>, alt!(
        conv!(SourceElement::Statement(statement))
        |
        conv!(SourceElement::FunctionDeclaration(function_declaration))
    ));

    named!(pub function_declaration<&str, FunctionDeclaration>, do_parse!(
        keyword!("function") >>
        name: js_identifier >>
        keyword!("(") >>
        formal_parameter_list: opt!(formal_parameter_list) >>
        keyword!(")") >>
        keyword!("{") >>
        body: opt!(function_body) >>
        keyword!("}") >>
        (FunctionDeclaration {
            name: name,
            formals: formal_parameter_list,
            body: body,
        })
    ));

    named!(pub formal_parameter_list<&str, FormalParameterList>, conv!(FormalParameterList(separated_nonempty_list!(
        keyword!(","),
        conv!(FormalParameter(js_identifier))
    ))));

    named!(pub function_body<&str, FunctionBody>, conv!(FunctionBody(source_elements)));

    #[cfg(test)]
    mod tests {
        use nom::{IResult, ErrorKind};
        use super::*;

        #[test]
        fn function_declaration() {
            assert!(super::function_declaration("").is_incomplete());

            {
                let name = "test";

                let input = format!("function {}() {{}} ", name);
                assert_eq!(
                    super::function_declaration(&input),
                    IResult::Done(" ", FunctionDeclaration {
                        name: super::js_identifier(name).unwrap().1,
                        formals: None,
                        body: None,
                    })
                );
            }

            {
                let name = "test";
                let formals = "test";
                let body = ";;";

                let input = format!("function {}({}) {{{}}} ", name, formals, body);
                assert_eq!(
                    super::function_declaration(&input),
                    IResult::Done(" ", FunctionDeclaration {
                        name: super::js_identifier(name).unwrap().1,
                        formals: Some(super::formal_parameter_list(formals).unwrap().1),
                        body: Some(super::function_body(body).unwrap().1),
                    })
                );
            }

            assert!(super::function_declaration(" function ").is_incomplete());
            assert!(super::function_declaration(" function test ").is_incomplete());
            assert!(super::function_declaration(" function test (").is_incomplete());
            assert!(super::function_declaration(" function test (test) ").is_incomplete());
            assert!(super::function_declaration(" function test (test) { ").is_incomplete());

            assert_eq!(
                super::function_declaration("function () {{}} "),
                IResult::Error(ErrorKind::Alt)
            );

            assert_eq!(
                super::function_declaration("function test {{}} "),
                IResult::Error(ErrorKind::Tag)
            );

            assert_eq!(
                super::function_declaration("function {{}} "),
                IResult::Error(ErrorKind::Alt)
            );
            
            assert_eq!(
                super::function_declaration("function test( {{}} "),
                IResult::Error(ErrorKind::Tag)
            );
        }
    }
}