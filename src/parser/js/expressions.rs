use std;
use parser::js::*;
use parser::js::terminals::*;


#[derive(Debug)]
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

#[derive(Debug)]
pub struct CallExpression<'a> {
    pub base: Box<Expression<'a>>,
    pub arguments: Option<ArgumentList<'a>>,
}

#[derive(Debug)]
pub struct ArgumentList<'a>(pub std::vec::Vec<Expression<'a>>);

#[derive(Debug)]
pub struct ConditionalExpression<'a> {
    pub expression: Box<Expression<'a>>,
    pub ok: Box<Expression<'a>>,
    pub ko: Box<Expression<'a>>,
}

#[derive(Debug)]
pub struct BinaryExpression<'a> {
    pub left: Box<Expression<'a>>,
    pub operator: &'a str,
    pub right: Box<Expression<'a>>,
}

#[derive(Debug)]
pub struct NotExpression<'a>(pub Box<Expression<'a>>);

#[derive(Debug)]
pub struct TildeExpression<'a>(pub Box<Expression<'a>>);

#[derive(Debug)]
pub struct UnaryMinusExpression<'a>(pub Box<Expression<'a>>);

#[derive(Debug)]
pub struct UnaryPlusExpression<'a>(pub Box<Expression<'a>>);

#[derive(Debug)]
pub struct PreDecrementExpression<'a>(pub Box<Expression<'a>>);

#[derive(Debug)]
pub struct PreIncrementExpression<'a>(pub Box<Expression<'a>>);

#[derive(Debug)]
pub struct TypeOfExpression<'a>(pub Box<Expression<'a>>);

#[derive(Debug)]
pub struct VoidExpression<'a>(pub Box<Expression<'a>>);

#[derive(Debug)]
pub struct DeleteExpression<'a>(pub Box<Expression<'a>>);

#[derive(Debug)]
pub struct PostDecrementExpression<'a>(pub Box<Expression<'a>>);

#[derive(Debug)]
pub struct PostIncrementExpression<'a>(pub Box<Expression<'a>>);

#[derive(Debug)]
pub struct FunctionExpression<'a> {
    pub name: Option<&'a str>,
    pub formals: Option<FormalParameterList<'a>>,
    pub body: Option<FunctionBody<'a>>,
}

#[derive(Debug)]
pub struct ArrayMemberExpression<'a> {
    pub base: Box<Expression<'a>>,
    pub expression: Box<Expression<'a>>,
}

#[derive(Debug)]
pub struct FieldMemberExpression<'a> {
    pub base: Box<Expression<'a>>,
    pub name: &'a str,
}

#[derive(Debug)]
pub struct NewMemberExpression<'a> {
    pub base: Box<Expression<'a>>,
    pub arguments: Option<ArgumentList<'a>>,
}

#[derive(Debug)]
pub struct ThisExpression;

#[derive(Debug)]
pub struct IdentifierExpression<'a>(pub &'a str);

#[derive(Debug)]
pub struct NullExpression;

#[derive(Debug)]
pub struct TrueLiteral;

#[derive(Debug)]
pub struct FalseLiteral;

#[derive(Debug)]
pub struct ArrayLiteral<'a> {
    pub elements: Option<ElementList<'a>>,
    pub elision: Option<Elision>,
}

#[derive(Debug)]
pub struct ElementList<'a>(pub std::vec::Vec<Element<'a>>);

#[derive(Debug)]
pub struct Element<'a> {
    pub elision: Option<Elision>,
    pub expression: Expression<'a>,
}

#[derive(Debug)]
pub struct Elision(pub usize);

#[derive(Debug)]
pub struct ObjectLiteral<'a> {
    pub properties: Option<PropertyAssignmentList<'a>>,
}

#[derive(Debug)]
pub struct PropertyAssignmentList<'a>(pub std::vec::Vec<PropertyAssignment<'a>>);

#[derive(Debug)]
pub enum PropertyAssignment<'a> {
    PropertyNameAndValue(PropertyNameAndValue<'a>),
    PropertyGetterSetter(PropertyGetterSetter<'a>),
}

#[derive(Debug)]
pub struct PropertyNameAndValue<'a> {
    pub name: PropertyName<'a>,
    pub value: Expression<'a>,
}

#[derive(Debug)]
pub struct PropertyGetterSetter<'a> {
    pub name: PropertyName<'a>,
    pub getter_setter_type: PropertyGetterSetterType,
    pub formals: Option<FormalParameterList<'a>>,
    pub function_body: Option<FunctionBody<'a>>,
}

#[derive(Debug)]
pub enum PropertyGetterSetterType {
    Getter,
    Setter,
}

#[derive(Debug)]
pub enum PropertyName<'a> {
    IdentifierPropertyName(IdentifierPropertyName<'a>),
    StringLiteralPropertyName(StringLiteralPropertyName<'a>),
    NumericLiteralPropertyName(NumericLiteralPropertyName),
}

#[derive(Debug)]
pub struct IdentifierPropertyName<'a>(pub &'a str);

#[derive(Debug)]
pub struct StringLiteralPropertyName<'a>(pub StringLiteral<'a>);

#[derive(Debug)]
pub struct NumericLiteralPropertyName(pub NumericLiteral);

#[derive(Debug)]
pub struct NestedExpression<'a>(pub Box<Expression<'a>>);

#[derive(Debug)]
pub struct ExpressionList<'a> {
    pub left: Box<Expression<'a>>,
    pub right: Box<Expression<'a>>,
}

#[derive(Debug)]
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
                                keyword!($op1)
                                $(
                                    | 
                                    keyword!($ops)
                                )*
                            ) >>
                            expr: $next >>
                            ((operator, expr))
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
                    keyword!(",") >>
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
                    keyword!(",") >>
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
            operator: assignment_operator >>
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
            operator: assignment_operator >>
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
            keyword!("?") >>
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
            keyword!("?") >>
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
    binary_op!(logical_or_expression_not_in, "||", logical_and_expression_not_in); // TODO: fix not_in when macros are allowed as idents

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
            keyword!("++") >>
            (Expression::PostIncrementExpression(PostIncrementExpression(Box::new(expression))))
        )
        |
        do_parse!(
            expression: left_hand_side_expression >>
            not!(line_terminator) >>
            keyword!("--") >>
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

    named!(call_expression<&str, Expression>, alt!(
        do_parse!(
            base: alt!(member_expression | call_expression) >>
            keyword!("(") >>
            arguments: opt!(argument_list) >>
            keyword!(")") >>
            (Expression::CallExpression(CallExpression {
                base: Box::new(base),
                arguments: arguments,
            }))
        )
        |
        do_parse!(
            base: call_expression >>
            keyword!("[") >>
            expression: expression_list >>
            keyword!("]") >>
            (Expression::ArrayMemberExpression(ArrayMemberExpression {
                base: Box::new(base),
                expression: Box::new(expression),
            }))
        )
        |
        do_parse!(
            base: call_expression >>
            keyword!(".") >>
            name: property_identifier >>
            (Expression::FieldMemberExpression(FieldMemberExpression {
                base: Box::new(base),
                name: name,
            }))
        )
    ));

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
                            keyword!("[") >>
                            expression: expression_list >>
                            keyword!("]") >>
                            ((Some(expression), None))
                        )
                        |
                        do_parse!(
                            keyword!(".") >>
                            name: property_identifier >>
                            ((None, Some(name)))
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
                            _ => panic!("Logic is broken"),
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
                value: value,
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

}