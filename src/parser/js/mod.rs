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

}