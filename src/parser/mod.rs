#[macro_use]
mod helpers;

mod ui;
mod js;

use parser::ui::*;
use parser::js::*;
use parser::js::statements::*;
use parser::js::expressions::*;

#[derive(Debug)]
pub enum TopLevel<'a> {
    UiProgram(UiProgram<'a>),
    Statement(Statement<'a>),
    ExpressionList(Expression<'a>),
    SourceElement(SourceElement<'a>),
    UiObjectMember(UiObjectMember<'a>),
    Program(Program<'a>),
}

pub mod parsing {
    use nom::{IResult, ErrorKind};
    use parser::*;
    use parser::helpers::parsing::*;
    use parser::ui::parsing::*;
    use parser::js::parsing::*;
    use parser::js::statements::parsing::*;
    use parser::js::expressions::parsing::*;

    pub fn parse(i: &str) -> Result<TopLevel, ErrorKind> {
        let res = do_parse!(i,
            ret: top_level >>
            take_while_s!(is_whitespace) >>
            (ret)
        );
        match res {
            IResult::Done(input, output) => {
                if input.len() != 0 {
                    return Err(ErrorKind::Custom(10));
                }

                Ok(output)
            },
            IResult::Incomplete(_) => Err(ErrorKind::Complete),
            IResult::Error(e) => Err(e),
        }
    }

    named!(top_level<&str, TopLevel>, alt!(
        conv!(TopLevel::UiProgram(ui_program))
        |
        conv!(TopLevel::Statement(statement))
        |
        conv!(TopLevel::ExpressionList(expression_list))
        |
        conv!(TopLevel::SourceElement(source_element))
        |
        conv!(TopLevel::UiObjectMember(ui_object_member))
        |
        conv!(TopLevel::Program(program))
    ));

}