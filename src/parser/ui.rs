use std;

use parser::js::*;
use parser::js::terminals::*;
use parser::js::statements::*;

#[derive(Debug)]
pub struct UiProgram<'a> {
    pub headers: Option<UiHeaderItemList<'a>>,
    pub members: UiRootMember<'a>,
}

#[derive(Debug)]
pub struct UiHeaderItemList<'a>(pub std::vec::Vec<UiHeaderItem<'a>>);

#[derive(Debug)]
pub enum UiHeaderItem<'a> {
    UiPragma(UiPragma<'a>),
    UiImport(UiImport<'a>),
}

#[derive(Debug)]
pub struct UiPragma<'a> {
    pub pragma_type: UiQualifiedPragmaId<'a>,
}

#[derive(Debug, PartialEq)]
pub struct UiQualifiedPragmaId<'a>(pub &'a str);

#[derive(Debug)]
pub struct UiImport<'a> {
    pub file: UiImportId<'a>,
    pub import_id: Option<&'a str>,
    pub version: Option<NumericLiteral>,
}

#[derive(Debug)]
pub enum UiImportId<'a> {
    UiQualifiedId(UiQualifiedId<'a>),
    StringLiteral(StringLiteral<'a>),
}

#[derive(Debug)]
pub struct UiRootMember<'a>(pub UiObjectMemberList<'a>);

#[derive(Debug)]
pub struct UiObjectMemberList<'a>(pub std::vec::Vec<UiObjectMember<'a>>);

#[derive(Debug)]
pub enum UiObjectMember<'a> {
    UiObjectDefinition(UiObjectDefinition<'a>),
    UiArrayBinding(UiArrayBinding<'a>),
    UiObjectBinding(UiObjectBinding<'a>),
    UiPublicMember(UiPublicMember<'a>),
    UiScriptBinding(UiScriptBinding<'a>),
    UiSourceElement(UiSourceElement<'a>),
}

#[derive(Debug)]
pub struct UiObjectDefinition<'a> {
    pub qualified_type_name_id: UiQualifiedId<'a>,
    pub initializer: UiObjectInitializer<'a>,
}

#[derive(Debug)]
pub struct UiObjectInitializer<'a> {
    pub members: Option<UiObjectMemberList<'a>>,
}

#[derive(Debug)]
pub struct UiArrayBinding<'a> {
    pub qualified_id: UiQualifiedId<'a>,
    pub members: UiArrayMemberList<'a>,
}

#[derive(Debug)]
pub struct UiArrayMemberList<'a>(pub std::vec::Vec<UiObjectMember<'a>>);

#[derive(Debug)]
pub struct UiObjectBinding<'a> {
    pub qualified_id: UiQualifiedId<'a>,
    pub qualified_type_name_id: UiQualifiedId<'a>,
    pub initializer: UiObjectInitializer<'a>,
}

#[derive(Debug)]
pub struct UiPublicMember<'a> {
    pub public_member_type: UiPublicMemberType,
    pub type_modifier: Option<&'a str>,
    pub member_type: Option<UiQualifiedId<'a>>,
    pub name: &'a str,
    pub statement: Option<Box<Statement<'a>>>,
    pub binding: Option<Box<UiObjectMember<'a>>>,
    pub is_default_member: bool,
    pub is_readonly_member: bool,
    pub parameters: Option<UiParameterList<'a>>,
}

#[derive(Debug)]
pub enum UiPublicMemberType {
    Signal,
    Property,
}

#[derive(Debug)]
pub struct UiParameterList<'a>(pub std::vec::Vec<UiParameter<'a>>);

#[derive(Debug)]
pub struct UiParameter<'a> {
    pub parameter_type: UiQualifiedId<'a>,
    pub name: &'a str,
}

#[derive(Debug)]
pub struct UiScriptBinding<'a> {
    pub qualified_id: UiQualifiedId<'a>,
    pub statement: Box<Statement<'a>>,
}

#[derive(Debug)]
pub enum UiSourceElement<'a> {
    FunctionDeclaration(FunctionDeclaration<'a>),
    VariableStatement(VariableStatement<'a>),
}

#[derive(Debug)]
pub struct UiQualifiedId<'a>(pub std::vec::Vec<&'a str>);

pub mod parsing {

    use nom::{IResult, ErrorKind};
    use parser::ui::*;
    use parser::helpers::parsing::*;
    use parser::js::parsing::*;
    use parser::js::terminals::parsing::*;
    use parser::js::expressions::*;
    use parser::js::expressions::parsing::*;
    use parser::js::statements::parsing::*;

    named!(pub ui_program<&str, UiProgram>, do_parse!(
        headers: opt!(ui_header_item_list) >>
        members: ui_root_member >>
        (UiProgram {
            headers: headers,
            members: members,
        })
    ));

    named!(ui_header_item_list<&str, UiHeaderItemList>, conv!(UiHeaderItemList(many1!(
        alt!(
            conv!(UiHeaderItem::UiPragma(ui_pragma))
            |
            conv!(UiHeaderItem::UiImport(ui_import))
        )
    ))));

    named!(ui_pragma<&str, UiPragma>, do_parse!(
        keyword!("pragma") >>
        pragma_type: ui_qualified_pragma_id >>
        automatic_semicolon >>
        (UiPragma {
            pragma_type: pragma_type
        })
    ));

    named!(ui_import<&str, UiImport>, do_parse!(
        keyword!("import") >>
        file: ui_import_id >>
        version: opt!(numeric_literal) >>
        import_id: opt!(do_parse!(
            keyword!("as") >>
            import_id: js_identifier >>
            (import_id)
        )) >>
        automatic_semicolon >>
        (UiImport {
            file: file,
            version: version,
            import_id: import_id,
        })
    ));

    named!(ui_import_id<&str, UiImportId>, alt!(
        conv!(UiImportId::StringLiteral(string_literal))
        |
        conv!(UiImportId::UiQualifiedId(ui_qualified_id))
    ));

    fn ui_qualified_pragma_id(i: &str) -> IResult<&str, UiQualifiedPragmaId> {
        let (new_i, expression) = try_parse!(i, member_expression);
        if let Expression::IdentifierExpression(ie) = expression {
            IResult::Done(new_i, UiQualifiedPragmaId(ie.0))
        } else {
            IResult::Error(ErrorKind::Custom(1))
        }
    }

    named!(ui_root_member<&str, UiRootMember>, do_parse!(
        ui_object_definition: ui_object_definition >>
        (UiRootMember(UiObjectMemberList(vec![UiObjectMember::UiObjectDefinition(ui_object_definition)])))
    ));

    named!(ui_object_definition<&str, UiObjectDefinition>, do_parse!(
        qualified_type_name_id: ui_qualified_id >>
        initializer: ui_object_initializer >>
        (UiObjectDefinition {
            qualified_type_name_id: qualified_type_name_id,
            initializer: initializer,
        })
    ));

    named!(ui_object_initializer<&str, UiObjectInitializer>, do_parse!(
        keyword!("{") >>
        members: opt!(ui_object_member_list) >>
        keyword!("}") >>
        (UiObjectInitializer {
            members: members
        })
    ));

    named!(ui_object_member_list<&str, UiObjectMemberList>, conv!(UiObjectMemberList(many1!(ui_object_member))));

    named!(pub ui_object_member<&str, UiObjectMember>, alt!(
        conv!(UiObjectMember::UiSourceElement(conv!(UiSourceElement::FunctionDeclaration(function_declaration))))
        |
        conv!(UiObjectMember::UiSourceElement(conv!(UiSourceElement::VariableStatement(variable_statement))))
        |
        do_parse!(
            keyword!("signal") >>
            name: identifier >>
            parameters: opt!(do_parse!(
                keyword!("(") >>
                parameters: opt!(ui_parameter_list) >>
                keyword!(")") >>
                (parameters)
            )) >>
            automatic_semicolon >>
            (UiObjectMember::UiPublicMember(UiPublicMember {
                name: name,
                type_modifier: None,
                parameters: match parameters {
                    Some(p) => p,
                    None => None,
                },
                member_type: None,
                public_member_type: UiPublicMemberType::Signal,
                statement: None,
                is_default_member: false,
                is_readonly_member: false,
                binding: None,
            }))
        )
        |
        do_parse!(
            keyword!("property") >>
            type_modifier: identifier >>
            keyword!("<") >>
            member_type: ui_property_type >>
            keyword!(">") >>
            name: js_identifier >>
            keyword!(":") >>
            keyword!("[") >>
            members: ui_array_member_list >>
            keyword!("]") >>
            (UiObjectMember::UiPublicMember(UiPublicMember {
                name: name,
                type_modifier: Some(type_modifier),
                parameters: None,
                member_type: Some(member_type),
                public_member_type: UiPublicMemberType::Property,
                statement: None,
                is_default_member: false,
                is_readonly_member: false,
                binding: Some(Box::new(UiObjectMember::UiArrayBinding(UiArrayBinding {
                    qualified_id: UiQualifiedId(vec![name]),
                    members: members,
                }))),
            }))
        )
        |
        do_parse!(
            default: opt!(keyword!("default")) >>
            keyword!("property") >>
            type_modifier: identifier >>
            keyword!("<") >>
            member_type: ui_property_type >>
            keyword!(">") >>
            name: js_identifier >>
            automatic_semicolon >>
            (UiObjectMember::UiPublicMember(UiPublicMember {
                name: name,
                type_modifier: Some(type_modifier),
                parameters: None,
                member_type: Some(member_type),
                public_member_type: UiPublicMemberType::Property,
                statement: None,
                is_default_member: default.is_some(),
                is_readonly_member: false,
                binding: None,
            }))
        )
        |
        do_parse!(
            readonly: opt!(keyword!("readonly")) >>
            keyword!("property") >>
            member_type: ui_property_type >>
            name: js_identifier >>
            keyword!(":") >>
            qualified_type_name_id: ui_qualified_id >>
            initializer: ui_object_initializer >>
            (UiObjectMember::UiPublicMember(UiPublicMember {
                name: name,
                type_modifier: None,
                parameters: None,
                member_type: Some(member_type),
                public_member_type: UiPublicMemberType::Property,
                statement: None,
                is_default_member: false,
                is_readonly_member: readonly.is_some(),
                binding: Some(Box::new(UiObjectMember::UiObjectBinding(UiObjectBinding {
                    qualified_id: UiQualifiedId(vec![name]),
                    qualified_type_name_id: qualified_type_name_id,
                    initializer: initializer,
                }))),
            }))
        )
        |
        do_parse!(
            default_readonly: opt!(alt!(
                keyword!("default")
                |
                keyword!("readonly")
            )) >>
            keyword!("property") >>
            member_type: ui_property_type >>
            name: js_identifier >>
            keyword!(":") >>
            statement: ui_script_statement >>
            ({
                let (default, readonly) = match default_readonly {
                    Some("default") => (true, false),
                    Some("readonly") => (false, true),
                    _ => (false, false)
                };
                UiObjectMember::UiPublicMember(UiPublicMember {
                    name: name,
                    type_modifier: None,
                    parameters: None,
                    member_type: Some(member_type),
                    public_member_type: UiPublicMemberType::Property,
                    statement: Some(Box::new(statement)),
                    is_default_member: default,
                    is_readonly_member: readonly,
                    binding: None,
                })
            })
        )
        |
        do_parse!(
            default: opt!(keyword!("default")) >>
            keyword!("property") >>
            member_type: ui_property_type >>
            name: js_identifier >>
            automatic_semicolon >>
            (UiObjectMember::UiPublicMember(UiPublicMember {
                name: name,
                type_modifier: None,
                parameters: None,
                member_type: Some(member_type),
                public_member_type: UiPublicMemberType::Property,
                statement: None,
                is_default_member: default.is_some(),
                is_readonly_member: false,
                binding: None,
            }))
        )
        |
        do_parse!(
            qualified_id: ui_qualified_id >>
            keyword!(":") >>
            statement: ui_script_statement >>
            (UiObjectMember::UiScriptBinding(UiScriptBinding {
                qualified_id: qualified_id,
                statement: Box::new(statement),
            }))
        )
        |
        conv!(UiObjectMember::UiObjectDefinition(ui_object_definition))
        |
        do_parse!(
            qualified_id: ui_qualified_id >>
            keyword!(":") >>
            keyword!("[") >>
            members: ui_array_member_list >>
            keyword!("]") >>
            (UiObjectMember::UiArrayBinding(UiArrayBinding {
                qualified_id: qualified_id,
                members: members,
            }))
        )
        |
        do_parse!(
            qualified_id: ui_qualified_id >>
            keyword!(":") >>
            qualified_type_name_id: ui_qualified_id >>
            initializer: ui_object_initializer >>
            (UiObjectMember::UiObjectBinding(UiObjectBinding {
                qualified_id: qualified_id,
                qualified_type_name_id: qualified_type_name_id,
                initializer: initializer,
            }))
        )
        |
        do_parse!(
            qualified_type_name_id: ui_qualified_id >>
            keyword!("on") >>
            qualified_id: ui_qualified_id >>
            initializer: ui_object_initializer >>
            (UiObjectMember::UiObjectBinding(UiObjectBinding {
                qualified_id: qualified_id,
                qualified_type_name_id: qualified_type_name_id,
                initializer: initializer,
            }))
        )
    ));

    named!(ui_script_statement<&str, Statement>, alt!(
        conv!(Statement::ExpressionStatement(expression_statement))
        |
        conv!(Statement::Block(block))
        |
        conv!(Statement::EmptyStatement(empty_statement))
        |
        conv!(Statement::IfStatement(if_statement))
        |
        conv!(Statement::WithStatement(with_statement))
        |
        conv!(Statement::SwitchStatement(switch_statement))
        |
        conv!(Statement::TryStatement(try_statement))
    ));

    named!(ui_array_member_list<&str, UiArrayMemberList>, conv!(UiArrayMemberList(separated_nonempty_list!(
        keyword!(","),
        conv!(UiObjectMember::UiObjectDefinition(ui_object_definition))
    ))));

    named!(ui_parameter_list<&str, UiParameterList>, conv!(UiParameterList(separated_nonempty_list!(
        keyword!(","),
        do_parse!(
            parameter_type: ui_property_type >>
            name: js_identifier >>
            (UiParameter {
                parameter_type: parameter_type,
                name: name,
            })
        )
    ))));

    named!(ui_property_type<&str, UiQualifiedId>, alt!(
        do_parse!(
            string: alt!(
                keyword!("var")
                |
                reserved_word
                |
                identifier
            ) >>
            (UiQualifiedId(vec![string]))
        )
        |
        conv!(UiQualifiedId(separated_nonempty_list!(
            keyword!("."),
            identifier
        )))
    ));

    named!(reserved_word<&str, &str>, alt!(
        keyword!("const")
        |
        keyword!("super")
        |
        keyword!("import")
    ));

    fn ui_qualified_id(i: &str) -> IResult<&str, UiQualifiedId> {
        let (new_i, expression) = try_parse!(i, member_expression);
        let mut expr_it = &expression;

        if let Expression::ArrayMemberExpression(ref ame) = *expr_it { //TODO: warning ignored annotation
            expr_it = &ame.base;
        };

        let mut names = vec![];

        while let Expression::FieldMemberExpression(ref fme) = *expr_it {
            names.push(fme.name);
            expr_it = &fme.base;
        }

        if let Expression::IdentifierExpression(ref ie) = *expr_it {
            names.push(ie.0);
            IResult::Done(new_i, UiQualifiedId(names))
        } else {
            IResult::Error(ErrorKind::Custom(0))
        }
    }

    #[cfg(test)]
    mod tests {
        use nom::IResult;
        use super::*;

        #[test]
        fn ui_qualified_pragma_id() {
            assert!(super::ui_qualified_pragma_id("").is_incomplete());

            assert_eq!(
                super::ui_qualified_pragma_id(" test "),
                IResult::Done(" ", UiQualifiedPragmaId("test"))
            );

            assert_eq!(
                super::ui_qualified_pragma_id(" test.field "),
                IResult::Error(ErrorKind::Custom(1))
            );
        }
    }

}