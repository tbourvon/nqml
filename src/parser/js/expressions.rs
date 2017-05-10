use std;
use parser::js::*;
use parser::js::terminals::*;


#[derive(Debug, PartialEq)]
pub enum Expression<'a> {
    CallExpression(CallExpression<'a>),
    ConditionalExpression(ConditionalExpression<'a>),
    BinaryExpression(BinaryExpression<'a>),
    NotExpression(NotExpression<'a>),
    TildeExpression(TildeExpression<'a>),
    UnaryMinusExpression(UnaryMinusExpression<'a>),
    UnaryPlusExpression(UnaryPlusExpression<'a>),
    PreDecrementExpression(PreDecrementExpression<'a>),
    PreIncrementExpression(PreIncrementExpression<'a>),
    TypeOfExpression(TypeOfExpression<'a>),
    VoidExpression(VoidExpression<'a>),
    DeleteExpression(DeleteExpression<'a>),
    PostDecrementExpression(PostDecrementExpression<'a>),
    PostIncrementExpression(PostIncrementExpression<'a>),
    FunctionExpression(FunctionExpression<'a>),
    ArrayMemberExpression(ArrayMemberExpression<'a>),
    FieldMemberExpression(FieldMemberExpression<'a>),
    NewMemberExpression(NewMemberExpression<'a>),
    ThisExpression(ThisExpression),
    IdentifierExpression(IdentifierExpression<'a>),
    NullExpression(NullExpression),
    TrueLiteral(TrueLiteral),
    FalseLiteral(FalseLiteral),
    NumericLiteral(NumericLiteral),
    //TODO: Multiline string literal
    StringLiteral(StringLiteral<'a>),
    //TODO: regexp
    ArrayLiteral(ArrayLiteral<'a>),
    ObjectLiteral(ObjectLiteral<'a>),
    NestedExpression(NestedExpression<'a>),
    ExpressionList(ExpressionList<'a>),
    NewExpression(NewExpression<'a>),
}

#[derive(Debug, PartialEq)]
pub struct CallExpression<'a> {
    pub base: Box<Expression<'a>>,
    pub arguments: Option<ArgumentList<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct ArgumentList<'a>(pub std::vec::Vec<Expression<'a>>);

#[derive(Debug, PartialEq)]
pub struct ConditionalExpression<'a> {
    pub expression: Box<Expression<'a>>,
    pub ok: Box<Expression<'a>>,
    pub ko: Box<Expression<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct BinaryExpression<'a> {
    pub left: Box<Expression<'a>>,
    pub operator: &'a str,
    pub right: Box<Expression<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct NotExpression<'a>(pub Box<Expression<'a>>);

#[derive(Debug, PartialEq)]
pub struct TildeExpression<'a>(pub Box<Expression<'a>>);

#[derive(Debug, PartialEq)]
pub struct UnaryMinusExpression<'a>(pub Box<Expression<'a>>);

#[derive(Debug, PartialEq)]
pub struct UnaryPlusExpression<'a>(pub Box<Expression<'a>>);

#[derive(Debug, PartialEq)]
pub struct PreDecrementExpression<'a>(pub Box<Expression<'a>>);

#[derive(Debug, PartialEq)]
pub struct PreIncrementExpression<'a>(pub Box<Expression<'a>>);

#[derive(Debug, PartialEq)]
pub struct TypeOfExpression<'a>(pub Box<Expression<'a>>);

#[derive(Debug, PartialEq)]
pub struct VoidExpression<'a>(pub Box<Expression<'a>>);

#[derive(Debug, PartialEq)]
pub struct DeleteExpression<'a>(pub Box<Expression<'a>>);

#[derive(Debug, PartialEq)]
pub struct PostDecrementExpression<'a>(pub Box<Expression<'a>>);

#[derive(Debug, PartialEq)]
pub struct PostIncrementExpression<'a>(pub Box<Expression<'a>>);

#[derive(Debug, PartialEq)]
pub struct FunctionExpression<'a> {
    pub name: Option<&'a str>,
    pub formals: Option<FormalParameterList<'a>>,
    pub body: Option<FunctionBody<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct ArrayMemberExpression<'a> {
    pub base: Box<Expression<'a>>,
    pub expression: Box<Expression<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct FieldMemberExpression<'a> {
    pub base: Box<Expression<'a>>,
    pub name: &'a str,
}

#[derive(Debug, PartialEq)]
pub struct NewMemberExpression<'a> {
    pub base: Box<Expression<'a>>,
    pub arguments: Option<ArgumentList<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct ThisExpression;

#[derive(Debug, PartialEq)]
pub struct IdentifierExpression<'a>(pub &'a str);

#[derive(Debug, PartialEq)]
pub struct NullExpression;

#[derive(Debug, PartialEq)]
pub struct TrueLiteral;

#[derive(Debug, PartialEq)]
pub struct FalseLiteral;

#[derive(Debug, PartialEq)]
pub struct ArrayLiteral<'a> {
    pub elements: Option<ElementList<'a>>,
    pub elision: Option<Elision>,
}

#[derive(Debug, PartialEq)]
pub struct ElementList<'a>(pub std::vec::Vec<Element<'a>>);

#[derive(Debug, PartialEq)]
pub struct Element<'a> {
    pub elision: Option<Elision>,
    pub expression: Expression<'a>,
}

#[derive(Debug, PartialEq)]
pub struct Elision(pub usize);

#[derive(Debug, PartialEq)]
pub struct ObjectLiteral<'a> {
    pub properties: Option<PropertyAssignmentList<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct PropertyAssignmentList<'a>(pub std::vec::Vec<PropertyAssignment<'a>>);

#[derive(Debug, PartialEq)]
pub enum PropertyAssignment<'a> {
    PropertyNameAndValue(PropertyNameAndValue<'a>),
    PropertyGetterSetter(PropertyGetterSetter<'a>),
}

#[derive(Debug, PartialEq)]
pub struct PropertyNameAndValue<'a> {
    pub name: PropertyName<'a>,
    pub value: Box<Expression<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct PropertyGetterSetter<'a> {
    pub name: PropertyName<'a>,
    pub getter_setter_type: PropertyGetterSetterType,
    pub formals: Option<FormalParameterList<'a>>,
    pub function_body: Option<FunctionBody<'a>>,
}

#[derive(Debug, PartialEq)]
pub enum PropertyGetterSetterType {
    Getter,
    Setter,
}

#[derive(Debug, PartialEq)]
pub enum PropertyName<'a> {
    IdentifierPropertyName(IdentifierPropertyName<'a>),
    StringLiteralPropertyName(StringLiteralPropertyName<'a>),
    NumericLiteralPropertyName(NumericLiteralPropertyName),
}

#[derive(Debug, PartialEq)]
pub struct IdentifierPropertyName<'a>(pub &'a str);

#[derive(Debug, PartialEq)]
pub struct StringLiteralPropertyName<'a>(pub StringLiteral<'a>);

#[derive(Debug, PartialEq)]
pub struct NumericLiteralPropertyName(pub NumericLiteral);

#[derive(Debug, PartialEq)]
pub struct NestedExpression<'a>(pub Box<Expression<'a>>);

#[derive(Debug, PartialEq)]
pub struct ExpressionList<'a> {
    pub left: Box<Expression<'a>>,
    pub right: Box<Expression<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct NewExpression<'a>(pub Box<Expression<'a>>);

pub mod parsing {

    macro_rules! binary_op {
        ($name:ident, [ $op1:tt $(, $ops:tt)* ], $next:ident) => (
            fn $name<'a>(i: &'a str) -> IResult<&'a str, Expression<'a>> {
                do_parse!(i,
                    first: $next >>
                    fold: fold_many0!(
                        do_parse!(
                            operator: alt!(
                                complete!(keyword!($op1))
                                $(
                                    | 
                                    complete!(keyword!($ops))
                                )*
                            ) >>
                            expr: $next >>
                            (operator, expr)
                        ),
                        first,
                        |acc: Expression<'a>, item: (&'a str, Expression<'a>)| {
                            Expression::BinaryExpression(BinaryExpression {
                                left: Box::new(acc),
                                operator: item.0,
                                right: Box::new(item.1),
                            })
                        }
                    ) >>
                    (fold)
                )
            }
        );
        ($name:ident, $op:tt, $next:ident) => (binary_op!($name, [$op], $next););
    }

    use nom::IResult;
    use parser::helpers::parsing::*;
    use parser::js::parsing::*;
    use parser::js::terminals::parsing::*;
    use super::*;

    pub fn expression_list<'a>(i: &'a str) -> IResult<&'a str, Expression<'a>> { // called Expression in qqmljs.g
        do_parse!(i,
            first: assignment_expression >>
            fold: fold_many0!(
                do_parse!(
                    complete!(keyword!(",")) >>
                    expr: assignment_expression >>
                    (expr)
                ),
                first,
                |acc: Expression<'a>, item: Expression<'a>| {
                    Expression::ExpressionList(ExpressionList {
                        left: Box::new(acc),
                        right: Box::new(item),
                    })
                }
            ) >>
            (fold)
        )
    }

    pub fn expression_list_not_in<'a>(i: &'a str) -> IResult<&'a str, Expression<'a>> { // called ExpressionNotIn in qqmljs.g
        do_parse!(i,
            first: assignment_expression_not_in >>
            fold: fold_many0!(
                do_parse!(
                    complete!(keyword!(",")) >>
                    expr: assignment_expression_not_in >>
                    (expr)
                ),
                first,
                |acc: Expression<'a>, item: Expression<'a>| {
                    Expression::ExpressionList(ExpressionList {
                        left: Box::new(acc),
                        right: Box::new(item),
                    })
                }
            ) >>
            (fold)
        )
    }

    named!(pub assignment_expression<&str, Expression>, alt!(
        do_parse!(
            left: left_hand_side_expression >>
            operator: complete!(assignment_operator) >>
            right: assignment_expression >>
            (Expression::BinaryExpression(BinaryExpression {
                left: Box::new(left),
                operator: operator,
                right: Box::new(right),
            }))
        )
        |
        conditional_expression
    ));

    named!(pub assignment_expression_not_in<&str, Expression>, alt!(
        do_parse!(
            left: left_hand_side_expression >>
            operator: complete!(assignment_operator) >>
            right: assignment_expression_not_in >>
            (Expression::BinaryExpression(BinaryExpression {
                left: Box::new(left),
                operator: operator,
                right: Box::new(right),
            }))
        )
        |
        conditional_expression_not_in
    ));

    named!(conditional_expression<&str, Expression>, alt!(
        do_parse!(
            expression: logical_or_expression >>
            complete!(keyword!("?")) >>
            ok: assignment_expression >>
            keyword!(":") >>
            ko: assignment_expression >>
            (Expression::ConditionalExpression(ConditionalExpression {
                expression: Box::new(expression),
                ok: Box::new(ok),
                ko: Box::new(ko),
            }))
        )
        |
        logical_or_expression
    ));

    named!(conditional_expression_not_in<&str, Expression>, alt!(
        do_parse!(
            expression: logical_or_expression_not_in >>
            complete!(keyword!("?")) >>
            ok: assignment_expression_not_in >>
            keyword!(":") >>
            ko: assignment_expression_not_in >>
            (Expression::ConditionalExpression(ConditionalExpression {
                expression: Box::new(expression),
                ok: Box::new(ok),
                ko: Box::new(ko),
            }))
        )
        |
        logical_or_expression
    ));

    binary_op!(logical_or_expression, "||", logical_and_expression);
    binary_op!(logical_or_expression_not_in, "||", logical_and_expression_not_in); // FIXME: not_in when macros are allowed as idents

    binary_op!(logical_and_expression, "&&", bitwise_or_expression);
    binary_op!(logical_and_expression_not_in, "&&", bitwise_or_expression_not_in);

    binary_op!(bitwise_or_expression, "|", bitwise_xor_expression);
    binary_op!(bitwise_or_expression_not_in, "|", bitwise_xor_expression_not_in);

    binary_op!(bitwise_xor_expression, "^", bitwise_and_expression);
    binary_op!(bitwise_xor_expression_not_in, "^", bitwise_and_expression_not_in);

    binary_op!(bitwise_and_expression, "&", equality_expression);
    binary_op!(bitwise_and_expression_not_in, "&", equality_expression_not_in);

    binary_op!(equality_expression, ["===", "!==", "==", "!="], relational_expression);
    binary_op!(equality_expression_not_in, ["===", "!==", "==", "!="], relational_expression_not_in);

    binary_op!(relational_expression, [">=", "<=", ">", "<", "instanceof", "in"], shift_expression);
    binary_op!(relational_expression_not_in, [">=", "<=", ">", "<", "instanceof"], shift_expression);

    binary_op!(shift_expression, [">>>", ">>", "<<"], additive_expression);

    binary_op!(additive_expression, ["+", "-"], multiplicative_expression);

    binary_op!(multiplicative_expression, ["*", "/", "%"], unary_expression);

    named!(unary_expression<&str, Expression>, alt!(
        do_parse!(
            keyword!("delete") >>
            expression: unary_expression >>
            (Expression::DeleteExpression(DeleteExpression(Box::new(expression))))
        )
        |
        do_parse!(
            keyword!("void") >>
            expression: unary_expression >>
            (Expression::VoidExpression(VoidExpression(Box::new(expression))))
        )
        |
        do_parse!(
            keyword!("typeof") >>
            expression: unary_expression >>
            (Expression::TypeOfExpression(TypeOfExpression(Box::new(expression))))
        )
        |
        do_parse!(
            keyword!("++") >>
            expression: unary_expression >>
            (Expression::PreIncrementExpression(PreIncrementExpression(Box::new(expression))))
        )
        |
        do_parse!(
            keyword!("--") >>
            expression: unary_expression >>
            (Expression::PreDecrementExpression(PreDecrementExpression(Box::new(expression))))
        )
        |
        do_parse!(
            keyword!("+") >>
            expression: unary_expression >>
            (Expression::UnaryPlusExpression(UnaryPlusExpression(Box::new(expression))))
        )
        |
        do_parse!(
            keyword!("-") >>
            expression: unary_expression >>
            (Expression::UnaryMinusExpression(UnaryMinusExpression(Box::new(expression))))
        )
        |
        do_parse!(
            keyword!("~") >>
            expression: unary_expression >>
            (Expression::TildeExpression(TildeExpression(Box::new(expression))))
        )
        |
        do_parse!(
            keyword!("!") >>
            expression: unary_expression >>
            (Expression::NotExpression(NotExpression(Box::new(expression))))
        )
        |
        postfix_expression
    ));

    named!(postfix_expression<&str, Expression>, alt!(
        do_parse!(
            expression: left_hand_side_expression >>
            not!(line_terminator) >>
            complete!(keyword!("++")) >>
            (Expression::PostIncrementExpression(PostIncrementExpression(Box::new(expression))))
        )
        |
        do_parse!(
            expression: left_hand_side_expression >>
            not!(line_terminator) >>
            complete!(keyword!("--")) >>
            (Expression::PostDecrementExpression(PostDecrementExpression(Box::new(expression))))
        )
        |
        left_hand_side_expression
    ));

    named!(pub left_hand_side_expression<&str, Expression>, alt!(
        new_expression
        |
        call_expression
    ));

    named!(new_expression<&str, Expression>, alt!(
        do_parse!(
            keyword!("new") >>
            expression: new_expression >>
            (Expression::NewExpression(NewExpression(Box::new(expression))))
        )
        |
        member_expression
    ));

    #[allow(cyclomatic_complexity)]
    fn call_expression<'a>(i: &'a str) -> IResult<&'a str, Expression<'a>> { 
        do_parse!(i,
            first: do_parse!(
                base: member_expression >>
                complete!(keyword!("(")) >>
                arguments: opt!(argument_list) >>
                keyword!(")") >>
                (Expression::CallExpression(CallExpression {
                    base: Box::new(base),
                    arguments: arguments,
                }))
            ) >>
            fold: fold_many0!(
                alt!(
                    do_parse!(
                        complete!(keyword!("(")) >>
                        arguments: opt!(argument_list) >>
                        keyword!(")") >>
                        (Some(arguments), None, None)
                    )
                    |
                    do_parse!(
                        complete!(keyword!("[")) >>
                        expression: expression_list >>
                        keyword!("]") >>
                        (None, Some(expression), None)
                    )
                    |
                    do_parse!(
                        complete!(keyword!(".")) >>
                        name: property_identifier >>
                        (None, None, Some(name))
                    )
                ),
                first,
                |acc: Expression<'a>, item: (Option<Option<ArgumentList<'a>>>, Option<Expression<'a>>, Option<&'a str>)| {
                    match item {
                        (Some(al), None, None) => Expression::CallExpression(CallExpression {
                            base: Box::new(acc),
                            arguments: al,
                        }),
                        (None, Some(e), None) => Expression::ArrayMemberExpression(ArrayMemberExpression {
                            base: Box::new(acc),
                            expression: Box::new(e),
                        }),
                        (None, None, Some(n)) => Expression::FieldMemberExpression(FieldMemberExpression {
                            base: Box::new(acc),
                            name: n,
                        }),
                        _ => panic!("call_expression is broken")
                    }
                }
            ) >>
            (fold)
        )
    }

    #[allow(cyclomatic_complexity)]
    pub fn member_expression<'a>(i: &'a str) -> IResult<&'a str, Expression<'a>> {
        alt!(i,
            do_parse!(
                keyword!("new") >>
                base: member_expression >>
                keyword!("(") >>
                arguments: opt!(argument_list) >>
                keyword!(")") >>
                (Expression::NewMemberExpression(NewMemberExpression {
                    base: Box::new(base),
                    arguments: arguments,
                }))
            )
            |
            do_parse!(
                first: alt!(
                    primary_expression
                    |
                    function_expression
                ) >>
                fold: fold_many0!(
                    alt!(
                        do_parse!(
                            complete!(keyword!("[")) >>
                            expression: expression_list >>
                            keyword!("]") >>
                            (Some(expression), None)
                        )
                        |
                        do_parse!(
                            complete!(keyword!(".")) >>
                            name: property_identifier >>
                            (None, Some(name))
                        )
                    ),
                    first,
                    |acc: Expression<'a>, item: (Option<Expression<'a>>, Option<&'a str>)| {
                        match item {
                            (Some(e), None) => {
                                Expression::ArrayMemberExpression(ArrayMemberExpression {
                                    base: Box::new(acc),
                                    expression: Box::new(e),
                                })
                            },
                            (None, Some(n)) => {
                                Expression::FieldMemberExpression(FieldMemberExpression {
                                    base: Box::new(acc),
                                    name: n,
                                })
                            },
                            _ => panic!("member_expression is broken"),
                        }
                    }
                ) >>
                (fold)
            )
        )
    }

    named!(argument_list<&str, ArgumentList>, conv!(ArgumentList(separated_nonempty_list!(
        keyword!(","),
        assignment_expression
    ))));

    named!(primary_expression<&str, Expression>, alt!(
        do_parse!(
            keyword!("this") >>
            (Expression::ThisExpression(ThisExpression))
        )
        |
        do_parse!(
            keyword!("null") >>
            (Expression::NullExpression(NullExpression))
        )
        |
        do_parse!(
            keyword!("true") >>
            (Expression::TrueLiteral(TrueLiteral))
        )
        |
        do_parse!(
            keyword!("false") >>
            (Expression::FalseLiteral(FalseLiteral))
        )
        |
        do_parse!(
            keyword!("[") >>
            elements: opt!(do_parse!(
                element_list: element_list >>
                opt!(keyword!(",")) >>
                (element_list)
            )) >>
            elision: opt!(elision) >>
            keyword!("]") >>
            (Expression::ArrayLiteral(ArrayLiteral {
                elements: elements,
                elision: elision,
            }))
        )
        |
        do_parse!(
            keyword!("{") >>
            properties: opt!(do_parse!(
                property_assignment_list: property_assignment_list >>
                opt!(keyword!(",")) >>
                (property_assignment_list)
            )) >>
            keyword!("}") >>
            (Expression::ObjectLiteral(ObjectLiteral {
                properties: properties,
            }))
        )
        |
        do_parse!(
            keyword!("(") >>
            expression: expression_list >>
            keyword!(")") >>
            (Expression::NestedExpression(NestedExpression(Box::new(expression))))
        )
        |
        conv!(Expression::NumericLiteral(numeric_literal))
        |
        conv!(Expression::StringLiteral(string_literal))
        |
        conv!(Expression::IdentifierExpression(conv!(IdentifierExpression(js_identifier))))
    ));

    named!(element_list<&str, ElementList>, conv!(ElementList(separated_nonempty_list!(
        keyword!(","),
        do_parse!(
            elision_opt: opt!(elision) >>
            assignment_expression: assignment_expression >>
            (Element {
                elision: elision_opt,
                expression: assignment_expression,
            })
        )
    ))));

    named!(elision<&str, Elision>, do_parse!(
        vec: many1!(keyword!(",")) >>
        (Elision(vec.len()))
    ));

    named!(property_assignment_list<&str, PropertyAssignmentList>, conv!(PropertyAssignmentList(separated_nonempty_list!(
        keyword!(","),
        property_assignment
    ))));

    named!(property_assignment<&str, PropertyAssignment>, alt!(
        do_parse!(
            keyword!("get") >>
            name: property_name >>
            keyword!("(") >>
            keyword!(")") >>
            keyword!("{") >>
            function_body: opt!(function_body) >>
            keyword!("}") >>
            (PropertyAssignment::PropertyGetterSetter(PropertyGetterSetter {
                name: name,
                getter_setter_type: PropertyGetterSetterType::Getter,
                formals: None,
                function_body: function_body,
            }))
        )
        |
        do_parse!(
            keyword!("set") >>
            name: property_name >>
            keyword!("(") >>
            formals: opt!(formal_parameter_list) >>
            keyword!(")") >>
            keyword!("{") >>
            function_body: opt!(function_body) >>
            keyword!("}") >>
            (PropertyAssignment::PropertyGetterSetter(PropertyGetterSetter {
                name: name,
                getter_setter_type: PropertyGetterSetterType::Setter,
                formals: formals,
                function_body: function_body,
            }))
        )
        |
        do_parse!(
            name: property_name >>
            keyword!(":") >>
            value: assignment_expression >>
            (PropertyAssignment::PropertyNameAndValue(PropertyNameAndValue {
                name: name,
                value: Box::new(value),
            }))
        )
    ));

    named!(property_name<&str, PropertyName>, alt!(
        conv!(PropertyName::IdentifierPropertyName(conv!(IdentifierPropertyName(reserved_identifier))))
        |
        conv!(PropertyName::IdentifierPropertyName(conv!(IdentifierPropertyName(js_identifier))))
        |
        conv!(PropertyName::StringLiteralPropertyName(conv!(StringLiteralPropertyName(string_literal))))
        |
        conv!(PropertyName::NumericLiteralPropertyName(conv!(NumericLiteralPropertyName(numeric_literal))))
    ));

    named!(function_expression<&str, Expression>, do_parse!(
        keyword!("function") >>
        name: opt!(js_identifier) >>
        keyword!("(") >>
        formals: opt!(formal_parameter_list) >>
        keyword!(")") >>
        keyword!("{") >>
        body: opt!(function_body) >>
        keyword!("}") >>
        (Expression::FunctionExpression(FunctionExpression {
            name: name,
            formals: formals,
            body: body,
        }))
    ));

    #[cfg(test)]
    mod tests {
        use nom::{ErrorKind};
        use super::*;

        #[test]
        fn expression_list() {
            assert!(super::expression_list("").is_incomplete());

            {
                let expression_list = "test";
                let assignment_expression = "test";

                let input = format!(" {}, {} ", expression_list, assignment_expression);

                assert_eq!(
                    super::expression_list(&input),
                    IResult::Done(" ", Expression::ExpressionList(ExpressionList {
                        left: Box::new(
                            super::expression_list(expression_list).unwrap().1
                        ),
                        right: Box::new(
                            super::assignment_expression(assignment_expression).unwrap().1
                        ),
                    }))
                );
            }

            assert!(super::expression_list(" test, ").is_incomplete());
        }

        #[test]
        fn expression_list_not_in() {
            assert!(super::expression_list_not_in("").is_incomplete());

            {
                let expression_list_not_in = "test";
                let assignment_expression_not_in = "test";

                let input = format!(" {}, {} ", expression_list_not_in, assignment_expression_not_in);

                assert_eq!(
                    super::expression_list_not_in(&input),
                    IResult::Done(" ", Expression::ExpressionList(ExpressionList {
                        left: Box::new(
                            super::expression_list_not_in(expression_list_not_in).unwrap().1
                        ),
                        right: Box::new(
                            super::assignment_expression_not_in(assignment_expression_not_in).unwrap().1
                        ),
                    }))
                );
            }

            assert!(super::expression_list_not_in(" test, ").is_incomplete());
        }

        #[test]
        fn assignment_expression() {
            assert!(super::assignment_expression("").is_incomplete());

            {
                let left_hand_side_expression = "test";
                let operator = "=";
                let assignment_expression = "test";

                let input = format!(" {} {} {} ", left_hand_side_expression, operator, assignment_expression);

                assert_eq!(
                    super::assignment_expression(&input),
                    IResult::Done(" ", Expression::BinaryExpression(BinaryExpression {
                        left: Box::new(
                            super::left_hand_side_expression(left_hand_side_expression).unwrap().1
                        ),
                        operator: super::assignment_operator(operator).unwrap().1,
                        right: Box::new(
                            super::assignment_expression(assignment_expression).unwrap().1
                        ),
                    }))
                );
            }

            assert!(super::assignment_expression(" test = ").is_incomplete());
        }

        #[test]
        fn assignment_expression_not_in() {
            assert!(super::assignment_expression_not_in("").is_incomplete());

            {
                let left_hand_side_expression = "test";
                let operator = "=";
                let assignment_expression_not_in = "test";

                let input = format!(" {} {} {} ", left_hand_side_expression, operator, assignment_expression_not_in);

                assert_eq!(
                    super::assignment_expression(&input),
                    IResult::Done(" ", Expression::BinaryExpression(BinaryExpression {
                        left: Box::new(
                            super::left_hand_side_expression(left_hand_side_expression).unwrap().1
                        ),
                        operator: super::assignment_operator(operator).unwrap().1,
                        right: Box::new(
                            super::assignment_expression_not_in(assignment_expression_not_in).unwrap().1
                        ),
                    }))
                );
            }

            assert!(super::assignment_expression_not_in(" test = ").is_incomplete());
        }

        #[test]
        fn conditional_expression() {
            assert!(super::conditional_expression("").is_incomplete());

            {
                let logical_or_expression = "test";
                let assignment_expression_true = "test";
                let assignment_expression_false = "test";

                let input = format!(" {} ? {} : {} ", logical_or_expression, assignment_expression_true, assignment_expression_false);

                assert_eq!(
                    super::conditional_expression(&input),
                    IResult::Done(" ", Expression::ConditionalExpression(ConditionalExpression {
                        expression: Box::new(
                            super::logical_or_expression(logical_or_expression).unwrap().1
                        ),
                        ok: Box::new(
                            super::assignment_expression(assignment_expression_true).unwrap().1
                        ),
                        ko: Box::new(
                            super::assignment_expression(assignment_expression_false).unwrap().1
                        ),
                    }))
                );
            }

            assert!(super::conditional_expression(" test ? ").is_incomplete());
            assert!(super::conditional_expression(" test ? test : ").is_incomplete());
        }

        #[test]
        fn conditional_expression_not_in() {
            assert!(super::conditional_expression_not_in("").is_incomplete());

            {
                let logical_or_expression_not_in = "test";
                let assignment_expression_true_not_in = "test";
                let assignment_expression_false_not_in = "test";

                let input = format!(" {} ? {} : {} ", logical_or_expression_not_in, assignment_expression_true_not_in, assignment_expression_false_not_in);

                assert_eq!(
                    super::conditional_expression_not_in(&input),
                    IResult::Done(" ", Expression::ConditionalExpression(ConditionalExpression {
                        expression: Box::new(
                            super::logical_or_expression_not_in(logical_or_expression_not_in).unwrap().1
                        ),
                        ok: Box::new(
                            super::logical_or_expression_not_in(assignment_expression_true_not_in).unwrap().1
                        ),
                        ko: Box::new(
                            super::logical_or_expression_not_in(assignment_expression_false_not_in).unwrap().1
                        ),
                    }))
                );
            }

            assert!(super::conditional_expression_not_in(" test ? ").is_incomplete());
            assert!(super::conditional_expression_not_in(" test ? test : ").is_incomplete());
        }

        #[test]
        fn binary_op() {
            assert!(super::logical_or_expression("").is_incomplete());

            // We test one binop for the others
            {
                let logical_or_expression = "test";
                let operator = "||";
                let logical_and_expression = "test";

                let input = format!(" {} {} {} ", logical_or_expression, operator, logical_and_expression);

                assert_eq!(
                    super::logical_or_expression(&input),
                    IResult::Done(" ", Expression::BinaryExpression(BinaryExpression {
                        left: Box::new(
                            super::logical_or_expression(logical_or_expression).unwrap().1
                        ),
                        operator: operator,
                        right: Box::new(
                            super::logical_and_expression(logical_and_expression).unwrap().1
                        ),
                    }))
                );
            }

            assert!(super::logical_or_expression(" test || ").is_incomplete());
        }

        #[test]
        fn postfix_expression() {
            assert!(super::postfix_expression("").is_incomplete());

            // ++
            {
                let left_hand_side_expression = "test";

                let input = format!(" {}++ ", left_hand_side_expression);

                assert_eq!(
                    super::postfix_expression(&input),
                    IResult::Done(" ", Expression::PostIncrementExpression(PostIncrementExpression(Box::new(
                        super::left_hand_side_expression(left_hand_side_expression).unwrap().1
                    ))))
                );
            }

            {
                let left_hand_side_expression = "true";

                let input = format!(" {}\n++ ", left_hand_side_expression);

                assert_eq!(
                    super::postfix_expression(&input),
                    IResult::Done("\n++ ", super::left_hand_side_expression(left_hand_side_expression).unwrap().1)
                );
            }

            // --
            {
                let left_hand_side_expression = "test";

                let input = format!(" {}-- ", left_hand_side_expression);

                assert_eq!(
                    super::postfix_expression(&input),
                    IResult::Done(" ", Expression::PostDecrementExpression(PostDecrementExpression(Box::new(
                        super::left_hand_side_expression(left_hand_side_expression).unwrap().1
                    ))))
                );
            }

            {
                let left_hand_side_expression = "true";

                let input = format!(" {}\n-- ", left_hand_side_expression);

                assert_eq!(
                    super::postfix_expression(&input),
                    IResult::Done("\n-- ", super::left_hand_side_expression(left_hand_side_expression).unwrap().1)
                );
            }
        }

        #[test]
        fn new_expression() {
            assert!(super::new_expression("").is_incomplete());

            {
                let member_expression = "test";

                let input = format!(" new {} ", member_expression);

                assert_eq!(
                    super::new_expression(&input),
                    IResult::Done(" ", Expression::NewExpression(NewExpression(Box::new(
                        super::member_expression(member_expression).unwrap().1
                    ))))
                );
            }

            assert!(super::new_expression(" new ").is_incomplete());
        }

        #[test]
        fn call_expression() {
            assert!(super::call_expression("").is_incomplete());

            // Simple call
            {
                let base = "test";

                let input = format!(" {}() ", base);

                assert_eq!(
                    super::call_expression(&input),
                    IResult::Done(" ", Expression::CallExpression(CallExpression {
                        base: Box::new(super::member_expression(base).unwrap().1),
                        arguments: None,
                    }))
                );
            }

            {
                let base = "test";
                let arguments = "test";

                let input = format!(" {}({}) ", base, arguments);

                assert_eq!(
                    super::call_expression(&input),
                    IResult::Done(" ", Expression::CallExpression(CallExpression {
                        base: Box::new(super::member_expression(base).unwrap().1),
                        arguments: Some(super::argument_list(arguments).unwrap().1),
                    }))
                );
            }

            assert!(super::call_expression(" test ( ").is_incomplete());

            // Chained call
            {
                let base = "test()";

                let input = format!(" {}() ", base);

                assert_eq!(
                    super::call_expression(&input),
                    IResult::Done(" ", Expression::CallExpression(CallExpression {
                        base: Box::new(super::call_expression(base).unwrap().1),
                        arguments: None,
                    }))
                );
            }

            {
                let base = "test()";
                let arguments = "test";

                let input = format!(" {}({}) ", base, arguments);

                assert_eq!(
                    super::call_expression(&input),
                    IResult::Done(" ", Expression::CallExpression(CallExpression {
                        base: Box::new(super::call_expression(base).unwrap().1),
                        arguments: Some(super::argument_list(arguments).unwrap().1),
                    }))
                );
            }

            assert!(super::call_expression(" test()( ").is_incomplete());

            // Chained array member
            {
                let base = "test()";
                let expression = "test";

                let input = format!(" {}[{}] ", base, expression);

                assert_eq!(
                    super::call_expression(&input),
                    IResult::Done(" ", Expression::ArrayMemberExpression(ArrayMemberExpression {
                        base: Box::new(super::call_expression(base).unwrap().1),
                        expression: Box::new(super::expression_list(expression).unwrap().1),
                    }))
                );
            }

            assert!(super::call_expression(" test()[ ").is_incomplete());

            // Chained field member
            {
                let base = "test()";
                let name = "test";

                let input = format!(" {}.{} ", base, name);

                assert_eq!(
                    super::call_expression(&input),
                    IResult::Done(" ", Expression::FieldMemberExpression(FieldMemberExpression {
                        base: Box::new(super::call_expression(base).unwrap().1),
                        name: super::property_identifier(name).unwrap().1,
                    }))
                );
            }

            assert!(super::call_expression(" test. ").is_incomplete());
        }

        #[test]
        fn member_expression() {
            assert!(super::member_expression("").is_incomplete());

            // New member expression
            {
                let base = "Test";

                let input = format!(" new {} () ", base);

                assert_eq!(
                    super::member_expression(&input),
                    IResult::Done(" ", Expression::NewMemberExpression(NewMemberExpression {
                        base: Box::new(super::member_expression(base).unwrap().1),
                        arguments: None,
                    }))
                );
            }

            {
                let base = "Test";
                let arguments = "test";

                let input = format!(" new {} ({}) ", base, arguments);

                assert_eq!(
                    super::member_expression(&input),
                    IResult::Done(" ", Expression::NewMemberExpression(NewMemberExpression {
                        base: Box::new(super::member_expression(base).unwrap().1),
                        arguments: Some(super::argument_list(arguments).unwrap().1),
                    }))
                );
            }

            assert!(super::member_expression(" new ").is_incomplete());
            assert!(super::member_expression(" new Test ").is_incomplete());
            assert!(super::member_expression(" new Test ( ").is_incomplete());

            // Array member expression
            {
                let base = "test";
                let expression = "test";

                let input = format!(" {}[{}] ", base, expression);

                assert_eq!(
                    super::member_expression(&input),
                    IResult::Done(" ", Expression::ArrayMemberExpression(ArrayMemberExpression {
                        base: Box::new(super::member_expression(base).unwrap().1),
                        expression: Box::new(super::expression_list(expression).unwrap().1),
                    }))
                );
            }

            assert!(super::member_expression(" test[ ").is_incomplete());

            // Field member expression
            {
                let base = "test";
                let name = "test";

                let input = format!(" {}.{} ", base, name);

                assert_eq!(
                    super::member_expression(&input),
                    IResult::Done(" ", Expression::FieldMemberExpression(FieldMemberExpression {
                        base: Box::new(super::member_expression(base).unwrap().1),
                        name: super::property_identifier(name).unwrap().1,
                    }))
                );
            }

            assert!(super::member_expression(" test. ").is_incomplete());
        }

        #[test]
        fn property_assignment() {
            assert!(super::property_assignment("").is_incomplete());

            // Getter
            {
                let name = "test";

                let input = format!(" get {} () {{}} ", name);

                assert_eq!(
                    super::property_assignment(&input),
                    IResult::Done(" ", PropertyAssignment::PropertyGetterSetter(PropertyGetterSetter {
                        name: super::property_name(name).unwrap().1,
                        getter_setter_type: PropertyGetterSetterType::Getter,
                        formals: None,
                        function_body: None,
                    }))
                );
            }

            {
                let name = "test";
                let function_body = ";;";

                let input = format!(" get {} () {{{}}} ", name, function_body);

                assert_eq!(
                    super::property_assignment(&input),
                    IResult::Done(" ", PropertyAssignment::PropertyGetterSetter(PropertyGetterSetter {
                        name: super::property_name(name).unwrap().1,
                        getter_setter_type: PropertyGetterSetterType::Getter,
                        formals: None,
                        function_body: Some(super::function_body(function_body).unwrap().1),
                    }))
                );
            }

            assert!(super::property_assignment(" get ").is_incomplete());
            assert!(super::property_assignment(" get test ").is_incomplete());
            assert!(super::property_assignment(" get test ( ").is_incomplete());
            assert!(super::property_assignment(" get test () { ").is_incomplete());

            assert_eq!(super::property_assignment(" get test ( {} "), IResult::Error(ErrorKind::Alt));
            assert_eq!(super::property_assignment(" get () {} "), IResult::Error(ErrorKind::Alt));

            // Setter
            {
                let name = "test";

                let input = format!(" set {} () {{}} ", name);

                assert_eq!(
                    super::property_assignment(&input),
                    IResult::Done(" ", PropertyAssignment::PropertyGetterSetter(PropertyGetterSetter {
                        name: super::property_name(name).unwrap().1,
                        getter_setter_type: PropertyGetterSetterType::Setter,
                        formals: None,
                        function_body: None,
                    }))
                );
            }

            {
                let name = "test";
                let formals = "test";
                let function_body = ";;";

                let input = format!(" set {} ({}) {{{}}} ", name, formals, function_body);

                assert_eq!(
                    super::property_assignment(&input),
                    IResult::Done(" ", PropertyAssignment::PropertyGetterSetter(PropertyGetterSetter {
                        name: super::property_name(name).unwrap().1,
                        getter_setter_type: PropertyGetterSetterType::Setter,
                        formals: Some(super::formal_parameter_list(formals).unwrap().1),
                        function_body: Some(super::function_body(function_body).unwrap().1),
                    }))
                );
            }

            assert!(super::property_assignment(" set ").is_incomplete());
            assert!(super::property_assignment(" set test ").is_incomplete());
            assert!(super::property_assignment(" set test ( ").is_incomplete());
            assert!(super::property_assignment(" set test () { ").is_incomplete());

            assert_eq!(super::property_assignment(" set test ( {} "), IResult::Error(ErrorKind::Alt));
            assert_eq!(super::property_assignment(" set () {} "), IResult::Error(ErrorKind::Alt));

            // Name and value
            {
                let name = "test";
                let value = "true";

                let input = format!(" {}: {} ", name, value);

                assert_eq!(
                    super::property_assignment(&input),
                    IResult::Done(" ", PropertyAssignment::PropertyNameAndValue(PropertyNameAndValue {
                        name: super::property_name(name).unwrap().1,
                        value: Box::new(super::assignment_expression(value).unwrap().1),
                    }))
                );
            }

            assert!(super::property_assignment(" test ").is_incomplete());
            assert!(super::property_assignment(" test: ").is_incomplete());

            assert_eq!(super::property_assignment(" : "), IResult::Error(ErrorKind::Alt));
        }

        #[test]
        fn primary_expression() {
            assert!(super::primary_expression("").is_incomplete());

            // Array literal
            assert_eq!(
                super::primary_expression(" [] "),
                IResult::Done(" ", Expression::ArrayLiteral(ArrayLiteral {
                    elements: None,
                    elision: None,
                }))
            );

            {
                let elements = ",,true,";
                let elision = ",,";

                let input = format!("[{}{}] ", elements, elision);
                assert_eq!(
                    super::primary_expression(&input),
                    IResult::Done(" ", Expression::ArrayLiteral(ArrayLiteral {
                        elements: Some(super::element_list(elements).unwrap().1),
                        elision: Some(super::elision(elision).unwrap().1),
                    }))
                );
            }

            assert!(super::primary_expression("[ ").is_incomplete());

            // Object literal
            assert_eq!(
                super::primary_expression(" {} "),
                IResult::Done(" ", Expression::ObjectLiteral(ObjectLiteral {
                    properties: None,
                }))
            );


            {
                let properties = "test: true"; 
                
                let input = format!("{{{}}} ", properties);
                assert_eq!(
                    super::primary_expression(&input),
                    IResult::Done(" ", Expression::ObjectLiteral(ObjectLiteral {
                        properties: Some(super::property_assignment_list(properties).unwrap().1),
                    }))
                );
            }


            assert!(super::primary_expression("{ ").is_incomplete());

            // Nested expressions
            {
                let expression = "true";

                let input = format!("({}) ", expression);
                assert_eq!(
                    super::primary_expression(&input),
                    IResult::Done(" ", Expression::NestedExpression(NestedExpression(Box::new(
                        super::expression_list(expression).unwrap().1
                    ))))
                );
            }

            assert!(super::primary_expression("( ").is_incomplete());

            assert_eq!(
                super::primary_expression(" () "),
                IResult::Error(ErrorKind::Alt)
            );
        }

        #[test]
        fn function_expression() {
            assert!(super::function_expression("").is_incomplete());

            assert_eq!(
                super::function_expression(" function () {} "),
                IResult::Done(" ", Expression::FunctionExpression(FunctionExpression {
                    name: None,
                    formals: None,
                    body: None,
                }))
            );

            {
                let name = "foo";
                let formals = "bar, test";
                let body = ";;";

                let input = format!("function {}({}) {{{}}} ", name, formals, body);
                assert_eq!(
                    super::function_expression(&input),
                    IResult::Done(" ", Expression::FunctionExpression(FunctionExpression {
                        name: Some(super::js_identifier(name).unwrap().1),
                        formals: Some(super::formal_parameter_list(formals).unwrap().1),
                        body: Some(super::function_body(body).unwrap().1),
                    }))
                );
            }

            assert!(super::function_expression("function () {{ ").is_incomplete());

            assert_eq!(
                super::function_expression("function ( {{}} "),
                IResult::Error(ErrorKind::Tag)
            );
        }
    }

}