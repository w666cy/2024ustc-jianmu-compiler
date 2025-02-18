#include "cminusf_builder.hpp"
#include "Value.hpp"

#define CONST_FP(num) ConstantFP::get((float)num, module.get())
#define CONST_INT(num) ConstantInt::get(num, module.get())

// types
Type *VOID_T;
Type *INT1_T;
Type *INT32_T;
Type *INT32PTR_T;
Type *FLOAT_T;
Type *FLOATPTR_T;

/*
 * use CMinusfBuilder::Scope to construct scopes
 * scope.enter: enter a new scope
 * scope.exit: exit current scope
 * scope.push: add a new binding to current scope
 * scope.find: find and return the value bound to the name
 */

Value *CminusfBuilder::visit(ASTProgram &node) {
    VOID_T = module->get_void_type();
    INT1_T = module->get_int1_type();
    INT32_T = module->get_int32_type();
    INT32PTR_T = module->get_int32_ptr_type();
    FLOAT_T = module->get_float_type();
    FLOATPTR_T = module->get_float_ptr_type();

    Value *ret_val = nullptr;
    for (auto &decl : node.declarations) {
        ret_val = decl->accept(*this);
    }
    return ret_val;
}

Value *CminusfBuilder::visit(ASTNum &node) {
    // TODO: This function is empty now.
    // Add some code here.
    if (node.type == TYPE_INT)
        context.tmp_val = CONST_INT(node.i_val);
    else
        context.tmp_val = CONST_FP(node.f_val);
    return nullptr;
}

Value *CminusfBuilder::visit(ASTVarDeclaration &node) {
    // TODO: This function is empty now.
    // Add some code here.
    Type *var_type;
    if (node.type == TYPE_INT)
        var_type = INT32_T;
    else
        var_type = FLOAT_T;
    if (node.num == nullptr) {
        if (scope.in_global()) {
            auto initializer = ConstantZero::get(var_type, module.get());
            auto var = GlobalVariable::create(node.id, module.get(), var_type,
                                              false, initializer);
            scope.push(node.id, var);
        } else {
            auto var = builder->create_alloca(var_type);
            scope.push(node.id, var);
        }
    } else {
        auto *array_type = ArrayType::get(var_type, node.num->i_val);
        if (scope.in_global()) {
            auto initializer = ConstantZero::get(array_type, module.get());
            auto var = GlobalVariable::create(node.id, module.get(), array_type,
                                              false, initializer);
            scope.push(node.id, var);
        } else {
            auto var = builder->create_alloca(array_type);
            scope.push(node.id, var);
        }
    }
    return nullptr;
}

Value *CminusfBuilder::visit(ASTFunDeclaration &node) {
    FunctionType *fun_type;
    Type *ret_type;
    std::vector<Type *> param_types;
    if (node.type == TYPE_INT)
        ret_type = INT32_T;
    else if (node.type == TYPE_FLOAT)
        ret_type = FLOAT_T;
    else
        ret_type = VOID_T;

    for (auto &param : node.params) {
        // TODO: Please accomplish param_types.
        if (param->type == TYPE_INT) {
            if (param->isarray) {
                param_types.push_back(INT32PTR_T);
            } else {
                param_types.push_back(INT32_T);
            }
        } else {
            if (param->isarray) {
                param_types.push_back(FLOATPTR_T);
            } else {
                param_types.push_back(FLOAT_T);
            }
        }
    }

    fun_type = FunctionType::get(ret_type, param_types);
    auto func = Function::create(fun_type, node.id, module.get());
    scope.push(node.id, func);
    context.func = func;
    auto funBB = BasicBlock::create(module.get(), "entry", func);
    builder->set_insert_point(funBB);
    scope.enter();
    std::vector<Value *> args;
    for (auto &arg : func->get_args()) {
        args.push_back(&arg);
    }
    Value *paramAlloc;
    for (int i = 0; i < node.params.size(); ++i) {
        // TODO: You need to deal with params and store them in the scope.
        if (node.params[i]->isarray) {
            if (node.params[i]->type == TYPE_INT)
                paramAlloc = builder->create_alloca(INT32PTR_T);
            else
                paramAlloc = builder->create_alloca(FLOATPTR_T);
            builder->create_store(args[i], paramAlloc);
            scope.push(node.params[i]->id, paramAlloc);
        } else {
            if (node.params[i]->type == TYPE_INT)
                paramAlloc = builder->create_alloca(INT32_T);
            else
                paramAlloc = builder->create_alloca(FLOAT_T);
            builder->create_store(args[i], paramAlloc);
            scope.push(node.params[i]->id, paramAlloc);
        }
    }
    node.compound_stmt->accept(*this);
    if (not builder->get_insert_block()->is_terminated()) {
        if (context.func->get_return_type()->is_void_type())
            builder->create_void_ret();
        else if (context.func->get_return_type()->is_float_type())
            builder->create_ret(CONST_FP(0.));
        else
            builder->create_ret(CONST_INT(0));
    }
    scope.exit();
    return nullptr;
}

Value *CminusfBuilder::visit(ASTParam &node) {
    // TODO: This function is empty now.
    // Add some code here.
    return nullptr;
}

Value *CminusfBuilder::visit(ASTCompoundStmt &node) {
    // TODO: This function is not complete.
    // You may need to add some code here
    // to deal with complex statements.
    scope.enter();

    for (auto &decl : node.local_declarations) {
        decl->accept(*this);
    }

    for (auto &stmt : node.statement_list) {
        stmt->accept(*this);
    }

    scope.exit();

    return nullptr;
}

Value *CminusfBuilder::visit(ASTExpressionStmt &node) {
    // TODO: This function is empty now.
    // Add some code here.
    if (node.expression != nullptr)
        node.expression->accept(*this);
    return nullptr;
}

Value *CminusfBuilder::visit(ASTSelectionStmt &node) {
    // TODO: This function is empty now.
    // Add some code here.
    node.expression->accept(*this);
    Value *ret_val = context.tmp_val;
    auto trueBB = BasicBlock::create(module.get(), "", context.func);
    BasicBlock *falseBB;
    // auto falseBB = BasicBlock::create(module.get(), "", context.func);
    auto contBB = BasicBlock::create(module.get(), "", context.func);
    Value *cond_val;
    if (ret_val->get_type()->is_integer_type())
        cond_val = builder->create_icmp_ne(ret_val, CONST_INT(0));
    else
        cond_val = builder->create_fcmp_ne(ret_val, CONST_FP(0.));
    if (node.else_statement == nullptr) {
        builder->create_cond_br(cond_val, trueBB, contBB);
    }
    else {
        falseBB = BasicBlock::create(module.get(), "", context.func);
        builder->create_cond_br(cond_val, trueBB, falseBB);
    }
    builder->set_insert_point(trueBB);
    node.if_statement->accept(*this);

    if (not builder->get_insert_block()->is_terminated())
        builder->create_br(contBB);

    if (node.else_statement != nullptr) {
        builder->set_insert_point(falseBB);
        node.else_statement->accept(*this);
        if (not builder->get_insert_block()->is_terminated())
            builder->create_br(contBB);
    }

    builder->set_insert_point(contBB);
    return nullptr;
}

Value *CminusfBuilder::visit(ASTIterationStmt &node) {
    // TODO: This function is empty now.
    // Add some code here.
    auto exprBB = BasicBlock::create(module.get(), "", context.func);
    auto trueBB = BasicBlock::create(module.get(), "", context.func);
    auto contBB = BasicBlock::create(module.get(), "", context.func);
    builder->create_br(exprBB); //
    builder->set_insert_point(exprBB);
    node.expression->accept(*this);
    Value *ret_val = context.tmp_val;
    Value *cond_val;
    if (ret_val->get_type()->is_integer_type())
        cond_val = builder->create_icmp_ne(ret_val, CONST_INT(0));
    else
        cond_val = builder->create_fcmp_ne(ret_val, CONST_FP(0.));

    builder->create_cond_br(cond_val, trueBB, contBB);
    builder->set_insert_point(trueBB);
    node.statement->accept(*this);
    if (not builder->get_insert_block()->is_terminated())
        builder->create_br(exprBB);
    builder->set_insert_point(contBB);
    return nullptr;
}

Value *CminusfBuilder::visit(ASTReturnStmt &node) {
    if (node.expression == nullptr) {
        builder->create_void_ret();
        return nullptr;
    } else {
        // TODO: The given code is incomplete.
        // You need to solve other return cases (e.g. return an integer).
        auto fun_ret_type =
            context.func->get_function_type()->get_return_type();
        node.expression->accept(*this);
        Value *ret_val = context.tmp_val;
        if (fun_ret_type != ret_val->get_type()) {
            if (fun_ret_type->is_integer_type())
                ret_val =
                    builder->create_fptosi(ret_val, INT32_T);
            else
                ret_val =
                    builder->create_sitofp(ret_val, FLOAT_T);
        }
        builder->create_ret(ret_val);
    }
    return nullptr;
}

Value *CminusfBuilder::visit(ASTVar &node) {
    // TODO: This function is empty now.
    // Add some code here.
    auto var = scope.find(node.id);
    auto is_int =
        var->get_type()->get_pointer_element_type()->is_integer_type();
    auto is_float =
        var->get_type()->get_pointer_element_type()->is_float_type();
    auto is_ptr =
        var->get_type()->get_pointer_element_type()->is_pointer_type();
    bool should_return_lvalue = context.require_lvalue;
    context.require_lvalue = false;
    if (node.expression == nullptr) {
        if (should_return_lvalue) {
            context.tmp_val = var;
        } else {
            if (is_int || is_float || is_ptr) {
                context.tmp_val = builder->create_load(var);
            } else {
                context.tmp_val =
                    builder->create_gep(var, {CONST_INT(0), CONST_INT(0)});
            }
        }
    } else {
        node.expression->accept(*this);
        auto val = context.tmp_val;
        auto exceptBB = BasicBlock::create(module.get(), "", context.func);
        auto contBB = BasicBlock::create(module.get(), "", context.func);
        if (val->get_type()->is_float_type())
            val = builder->create_fptosi(val, INT32_T);

        Value *is_neg = builder->create_icmp_lt(val, CONST_INT(0));

        builder->create_cond_br(is_neg, exceptBB, contBB);
        builder->set_insert_point(exceptBB);
        auto neg_idx_except_fun = scope.find("neg_idx_except");
        builder->create_call(static_cast<Function *>(neg_idx_except_fun), {});
        if (context.func->get_return_type()->is_void_type())
            builder->create_void_ret();
        else if (context.func->get_return_type()->is_float_type())
            builder->create_ret(CONST_FP(0.));
        else
            builder->create_ret(CONST_INT(0));

        builder->set_insert_point(contBB);
        Value *tmp_ptr;
        if (is_int || is_float)
            tmp_ptr = builder->create_gep(var, {val});
        else if (is_ptr) {
            auto array_load = builder->create_load(var);
            tmp_ptr = builder->create_gep(array_load, {val});
        } else
            tmp_ptr = builder->create_gep(var, {CONST_INT(0), val});
        if (should_return_lvalue) {
            context.tmp_val = tmp_ptr;
            context.require_lvalue = false;
        } else {
            context.tmp_val = builder->create_load(tmp_ptr);
        }
    }
    return nullptr;
}

Value *CminusfBuilder::visit(ASTAssignExpression &node) {
    // TODO: This function is empty now.
    // Add some code here.
    node.expression->accept(*this);
    Value *expr_result = context.tmp_val;
    context.require_lvalue = true;
    node.var->accept(*this);
    Value *var_addr = context.tmp_val;
    if (var_addr->get_type()->get_pointer_element_type() !=
        expr_result->get_type()) {
        if (expr_result->get_type() == INT32_T)
            expr_result = builder->create_sitofp(expr_result, FLOAT_T);
        else
            expr_result = builder->create_fptosi(expr_result, INT32_T);
    }
    builder->create_store(expr_result, var_addr);
    context.tmp_val = expr_result;
    return nullptr;
}

Value *CminusfBuilder::visit(ASTSimpleExpression &node) {
    if (node.additive_expression_r == nullptr) {
        node.additive_expression_l->accept(*this);
    } else {
        node.additive_expression_l->accept(*this);
        Value *l_val = context.tmp_val;
        node.additive_expression_r->accept(*this);
        Value *r_val = context.tmp_val;
        bool is_int = false;
        if (l_val->get_type()->is_integer_type()) {
            if (r_val->get_type()->is_integer_type())
                is_int = true;
            else
                l_val = builder->create_sitofp(l_val, FLOAT_T);
        }
        else {
            if (r_val->get_type()->is_integer_type())
                r_val = builder->create_sitofp(r_val, FLOAT_T);
        }
        Value *cmp;
        switch (node.op) {
        case OP_LT:
            if (is_int)
                cmp = builder->create_icmp_lt(l_val, r_val);
            else
                cmp = builder->create_fcmp_lt(l_val, r_val);
            break;
        case OP_LE:
            if (is_int)
                cmp = builder->create_icmp_le(l_val, r_val);
            else
                cmp = builder->create_fcmp_le(l_val, r_val);
            break;
        case OP_GE:
            if (is_int)
                cmp = builder->create_icmp_ge(l_val, r_val);
            else
                cmp = builder->create_fcmp_ge(l_val, r_val);
            break;
        case OP_GT:
            if (is_int)
                cmp = builder->create_icmp_gt(l_val, r_val);
            else
                cmp = builder->create_fcmp_gt(l_val, r_val);
            break;
        case OP_EQ:
            if (is_int)
                cmp = builder->create_icmp_eq(l_val, r_val);
            else
                cmp = builder->create_fcmp_eq(l_val, r_val);
            break;
        case OP_NEQ:
            if (is_int)
                cmp = builder->create_icmp_ne(l_val, r_val);
            else
                cmp = builder->create_fcmp_ne(l_val, r_val);
            break;
        }
        context.tmp_val = builder->create_zext(cmp, INT32_T);
    }
    return nullptr;
}

Value *CminusfBuilder::visit(ASTAdditiveExpression &node) {
    // TODO: This function is empty now.
    // Add some code here.
    if (node.additive_expression == nullptr) {
        node.term->accept(*this);
    } else {
        node.additive_expression->accept(*this);
        auto l_val = context.tmp_val;
        node.term->accept(*this);
        auto r_val = context.tmp_val;
        bool is_int = false;
        if (l_val->get_type()->is_integer_type()) {
            if (r_val->get_type()->is_integer_type())
                is_int = true;
            else
                l_val = builder->create_sitofp(l_val, FLOAT_T);
        }
        else {
            if (r_val->get_type()->is_integer_type())
                r_val = builder->create_sitofp(r_val, FLOAT_T);
        }
        switch (node.op) {
        case OP_PLUS:
            if (is_int)
                context.tmp_val = builder->create_iadd(l_val, r_val);
            else
                context.tmp_val = builder->create_fadd(l_val, r_val);
            break;
        case OP_MINUS:
            if (is_int)
                context.tmp_val = builder->create_isub(l_val, r_val);
            else
                context.tmp_val = builder->create_fsub(l_val, r_val);
            break;
        }
    }
    return nullptr;
}

Value *CminusfBuilder::visit(ASTTerm &node) {
    // TODO: This function is empty now.
    // Add some code here.
    if (node.term == nullptr) {
        node.factor->accept(*this);
    } else {
        node.term->accept(*this);
        auto l_val = context.tmp_val;
        node.factor->accept(*this);
        auto r_val = context.tmp_val;
        bool is_int = false;
        if (l_val->get_type()->is_integer_type()) {
            if (r_val->get_type()->is_integer_type())
                is_int = true;
            else
                l_val = builder->create_sitofp(l_val, FLOAT_T);
        }
        else {
            if (r_val->get_type()->is_integer_type())
                r_val = builder->create_sitofp(r_val, FLOAT_T);
        }
        switch (node.op) {
        case OP_MUL:
            if (is_int)
                context.tmp_val = builder->create_imul(l_val, r_val);
            else
                context.tmp_val = builder->create_fmul(l_val, r_val);
            break;
        case OP_DIV:
            if (is_int)
                context.tmp_val = builder->create_isdiv(l_val, r_val);
            else
                context.tmp_val = builder->create_fdiv(l_val, r_val);
            break;
        }
    }
    return nullptr;
}

Value *CminusfBuilder::visit(ASTCall &node) {
    // TODO: This function is empty now.
    // Add some code here.
    auto fun = static_cast<Function *>(scope.find(node.id));
    std::vector<Value *> args;
    auto param_type = fun->get_function_type()->param_begin();
    for (auto &arg : node.args) {
        arg->accept(*this);
        if (!context.tmp_val->get_type()->is_pointer_type() &&
            *param_type != context.tmp_val->get_type()) {
            if (context.tmp_val->get_type()->is_integer_type())
                context.tmp_val =
                    builder->create_sitofp(context.tmp_val, FLOAT_T);
            else
                context.tmp_val =
                    builder->create_fptosi(context.tmp_val, INT32_T);
        }
        args.push_back(context.tmp_val);
        param_type++;
    }

    context.tmp_val = builder->create_call(static_cast<Function *>(fun), args);
    return nullptr;
}

// #include "cminusf_builder.hpp"
// #include "BasicBlock.hpp"
// #include "Type.hpp"
// #include "Value.hpp"
// #include "ast.hpp"

// #define CONST_FP(num) ConstantFP::get((float)num, module.get())
// #define CONST_INT(num) ConstantInt::get(num, module.get())

// // types
// Type *VOID_T;
// Type *INT1_T;
// Type *INT32_T;
// Type *INT32PTR_T;
// Type *FLOAT_T;
// Type *FLOATPTR_T;

// /*
//  * use CMinusfBuilder::Scope to construct scopes
//  * scope.enter: enter a new scope
//  * scope.exit: exit current scope
//  * scope.push: add a new binding to current scope
//  * scope.find: find and return the value bound to the name
//  */

// Value* CminusfBuilder::visit(ASTProgram &node) {
//     VOID_T = module->get_void_type();
//     INT1_T = module->get_int1_type();
//     INT32_T = module->get_int32_type();
//     INT32PTR_T = module->get_int32_ptr_type();
//     FLOAT_T = module->get_float_type();
//     FLOATPTR_T = module->get_float_ptr_type();

//     Value *ret_val = nullptr;
//     for (auto &decl : node.declarations) {
//         ret_val = decl->accept(*this);
//     }
//     return ret_val;
// }

// Value* CminusfBuilder::visit(ASTNum &node) {
//     // TODO: This function is empty now.
//     // Add some code here.
//     if (node.type == TYPE_INT)
//         context.tmp_val = CONST_INT(node.i_val);
//     else
//         context.tmp_val = CONST_FP(node.f_val);
//     return nullptr;
// }

// Value* CminusfBuilder::visit(ASTVarDeclaration &node) {
//     // TODO: This function is empty now.
//     // Add some code here.
//     Type *VarType;
//     if (node.type == TYPE_INT)
//         VarType = INT32_T;
//     else
//         VarType = FLOAT_T;

//     if (node.num == nullptr) {
//         if (scope.in_global()) {
//             auto initializer = ConstantZero::get(VarType, module.get());
//             auto Var =
//                 GlobalVariable::create(node.id, module.get(), VarType, false,
//                 initializer); //
//             scope.push(node.id, Var);
//         }
//         else {
//             auto Var = builder->create_alloca(VarType);
//             scope.push(node.id, Var);
//         }
//     }
//     else {
//         auto ArrayType = ArrayType::get(VarType, node.num->i_val);
//         if (scope.in_global()) {
//             auto initializer = ConstantZero::get(ArrayType, module.get());
//             auto Var =
//                 GlobalVariable::create(node.id, module.get(), ArrayType,
//                 false, initializer); //
//             scope.push(node.id, Var);
//         }
//         else {
//             auto Var = builder->create_alloca(ArrayType);
//             scope.push(node.id, Var);
//         }
//     }
//     return nullptr;
// }

// Value* CminusfBuilder::visit(ASTFunDeclaration &node) {
//     FunctionType *fun_type;
//     Type *ret_type;
//     std::vector<Type *> param_types;
//     if (node.type == TYPE_INT)
//         ret_type = INT32_T;
//     else if (node.type == TYPE_FLOAT)
//         ret_type = FLOAT_T;
//     else
//         ret_type = VOID_T;

//     for (auto &param : node.params) {
//         // TODO: Please accomplish param_types.
//         if (param->type == TYPE_INT) {
//             if (param->isarray)
//                 param_types.push_back(INT32PTR_T);
//             else
//                 param_types.push_back(INT32_T);
//         }
//         else {
//             if (param->isarray)
//                 param_types.push_back(FLOATPTR_T);
//             else
//                 param_types.push_back(FLOAT_T);
//         }
//     }

//     fun_type = FunctionType::get(ret_type, param_types);
//     auto func = Function::create(fun_type, node.id, module.get());
//     scope.push(node.id, func);
//     context.func = func;
//     auto funBB = BasicBlock::create(module.get(), "entry", func);
//     builder->set_insert_point(funBB);
//     scope.enter();
//     context.pre_enter_scope = true; //
//     std::vector<Value *> args;
//     for (auto &arg : func->get_args()) {
//         args.push_back(&arg);
//     }
//     for (unsigned int i = 0; i < node.params.size(); ++i) {
//         // TODO: You need to deal with params and store them in the scope.
//         Value *paramAlloca;
//         if (node.params[i]->isarray) {
//             if (node.params[i]->type == TYPE_INT)
//                 paramAlloca = builder->create_alloca(INT32PTR_T);
//             else
//                 paramAlloca = builder->create_alloca(FLOATPTR_T);
//             builder->create_store(args[i], paramAlloca);
//             scope.push(node.params[i]->id, paramAlloca);
//         }
//         else {
//             if (node.params[i]->type == TYPE_INT)
//                 paramAlloca = builder->create_alloca(INT32_T);
//             else
//                 paramAlloca = builder->create_alloca(FLOAT_T);
//             builder->create_store(args[i], paramAlloca);
//             scope.push(node.params[i]->id, paramAlloca);
//         }
//     }
//     node.compound_stmt->accept(*this);
//     if (not builder->get_insert_block()->is_terminated())
//     {
//         if (context.func->get_return_type()->is_void_type())
//             builder->create_void_ret();
//         else if (context.func->get_return_type()->is_float_type())
//             builder->create_ret(CONST_FP(0.));
//         else
//             builder->create_ret(CONST_INT(0));
//     }
//     scope.exit();
//     return nullptr;
// }

// Value* CminusfBuilder::visit(ASTParam &node) {
//     // TODO: This function is empty now.
//     // Add some code here.
//     return nullptr;
// }

// Value* CminusfBuilder::visit(ASTCompoundStmt &node) {
//     // TODO: This function is not complete.
//     // You may need to add some code here
//     // to deal with complex statements.
//     bool exit_scope = !context.pre_enter_scope;
//     if (context.pre_enter_scope)
//         context.pre_enter_scope = false;
//     else
//         scope.enter();

//     for (auto &decl : node.local_declarations) {
//         decl->accept(*this);
//     }

//     for (auto &stmt : node.statement_list) {
//         stmt->accept(*this);
//         if (builder->get_insert_block()->is_terminated())
//             break;
//     }

//     if (exit_scope)
//         scope.exit();
//     return nullptr;
// }

// Value* CminusfBuilder::visit(ASTExpressionStmt &node) {
//     // TODO: This function is empty now.
//     // Add some code here.
//     if (node.expression != nullptr)
//         node.expression->accept(*this);
//     return nullptr;
// }

// Value* CminusfBuilder::visit(ASTSelectionStmt &node) {
//     // TODO: This function is empty now.
//     // Add some code here.
//     node.expression->accept(*this);
//     Value *expr_val = context.tmp_val;

//     auto trueBB = BasicBlock::create(module.get(), "trueBB", context.func);
//     // auto falseBB = BasicBlock::create(module.get(), "falseBB",
//     context.func); BasicBlock *falseBB; auto nextBB =
//     BasicBlock::create(module.get(), "nextBB", context.func);

//     Value *cond_val;
//     if (expr_val->get_type()->is_float_type())
//         cond_val = builder->create_fcmp_ne(expr_val, CONST_FP(0.));
//     else
//         cond_val = builder->create_icmp_ne(expr_val, CONST_INT(0));

//     if (node.else_statement == nullptr)
//         builder->create_cond_br(cond_val, trueBB, nextBB);
//     else {
//         falseBB = BasicBlock::create(module.get(), "falseBB", context.func);
//         builder->create_cond_br(cond_val, trueBB, falseBB);
//     }

//     builder->set_insert_point(trueBB);
//     node.if_statement->accept(*this);
//     if (not builder->get_insert_block()->is_terminated())
//         builder->create_br(nextBB); //

//     if (node.else_statement != nullptr) {
//         builder->set_insert_point(falseBB);
//         node.else_statement->accept(*this);
//         if (not builder->get_insert_block()->is_terminated())
//             builder->create_br(nextBB); //
//     }

//     builder->set_insert_point(nextBB);

//     return nullptr;
// }

// Value* CminusfBuilder::visit(ASTIterationStmt &node) {
//     // TODO: This function is empty now.
//     // Add some code here.
//     auto exprBB = BasicBlock::create(module.get(), "exprBB", context.func);
//     auto stmtBB = BasicBlock::create(module.get(), "stmtBB", context.func);
//     auto nextBB = BasicBlock::create(module.get(), "nextBB", context.func);

//     if (not builder->get_insert_block()->is_terminated())
//         builder->create_br(exprBB); //
//     builder->set_insert_point(exprBB);
//     node.expression->accept(*this);
//     Value *expr_val = context.tmp_val;

//     Value *cond_val;
//     if (expr_val->get_type()->is_float_type())
//         cond_val = builder->create_fcmp_ne(expr_val, CONST_FP(0.));
//     else
//         cond_val = builder->create_icmp_ne(expr_val, CONST_INT(0));
//     builder->create_cond_br(cond_val, stmtBB, nextBB);

//     builder->set_insert_point(stmtBB);
//     node.statement->accept(*this);
//     if (not builder->get_insert_block()->is_terminated())
//         builder->create_br(exprBB); //

//     builder->set_insert_point(nextBB);

//     return nullptr;
// }

// Value* CminusfBuilder::visit(ASTReturnStmt &node) {
//     if (node.expression == nullptr) {
//         builder->create_void_ret();
//         return nullptr;
//     } else {
//         // TODO: The given code is incomplete.
//         // You need to solve other return cases (e.g. return an integer).
//         auto ret_type = context.func->get_function_type()->get_return_type();
//         // node.expression->accept(*this);
//         // Value *expr_val = context.tmp_val;
//         if (ret_type != context.tmp_val->get_type()) {
//             if (ret_type->is_integer_type())
//                 context.tmp_val = builder->create_fptosi(context.tmp_val,
//                 INT32_T);
//             else
//                 context.tmp_val = builder->create_sitofp(context.tmp_val,
//                 FLOAT_T);
//         }
//         builder->create_ret(context.tmp_val);
//     }
//     return nullptr;
// }

// Value* CminusfBuilder::visit(ASTVar &node) {
//     // TODO: This function is empty now.
//     // Add some code here.
//     auto var = scope.find(node.id);
//     assert(var != nullptr);
//     auto is_int =
//     var->get_type()->get_pointer_element_type()->is_integer_type(); auto
//     is_float = var->get_type()->get_pointer_element_type()->is_float_type();
//     auto is_ptr =
//     var->get_type()->get_pointer_element_type()->is_pointer_type(); bool
//     should_return_lvalue = context.require_lvalue; context.require_lvalue =
//     false; if (node.expression == nullptr) {
//         if (should_return_lvalue) {
//             context.tmp_val = var;
//             context.require_lvalue = false;
//         } else {
//             if (is_int || is_float || is_ptr) {
//                 context.tmp_val = builder->create_load(var);
//             } else {
//                 context.tmp_val = builder->create_gep(var, {CONST_INT(0),
//                 CONST_INT(0)});
//             }
//         }
//     } else {
//         node.expression->accept(*this);
//         auto val = context.tmp_val;
//         Value *is_neg;
//         auto exceptBB = BasicBlock::create(module.get(), "", context.func);
//         auto contBB = BasicBlock::create(module.get(), "", context.func);
//         if (val->get_type()->is_float_type())
//             val = builder->create_fptosi(val, INT32_T);

//         is_neg = builder->create_icmp_lt(val, CONST_INT(0));

//         builder->create_cond_br(is_neg, exceptBB, contBB);
//         builder->set_insert_point(exceptBB);
//         auto neg_idx_except_fun = scope.find("neg_idx_except");
//         builder->create_call(static_cast<Function *>(neg_idx_except_fun),
//         {}); if (context.func->get_return_type()->is_void_type())
//             builder->create_void_ret();
//         else if (context.func->get_return_type()->is_float_type())
//             builder->create_ret(CONST_FP(0.));
//         else
//             builder->create_ret(CONST_INT(0));

//         builder->set_insert_point(contBB);
//         Value *tmp_ptr;
//         if (is_int || is_float)
//             tmp_ptr = builder->create_gep(var, {val});
//         else if (is_ptr) {
//             auto array_load = builder->create_load(var);
//             tmp_ptr = builder->create_gep(array_load, {val});
//         } else
//             tmp_ptr = builder->create_gep(var, {CONST_INT(0), val});
//         if (should_return_lvalue) {
//             context.tmp_val = tmp_ptr;
//             context.require_lvalue = false;
//         } else {
//             context.tmp_val = builder->create_load(tmp_ptr);
//         }
//     }
//     return nullptr;
// }

// Value* CminusfBuilder::visit(ASTAssignExpression &node) {
//     // TODO: This function is empty now.
//     // Add some code here.
//     node.expression->accept(*this);
//     Value *expr_val = context.tmp_val;

//     context.require_lvalue = true;
//     node.var->accept(*this);
//     Value *var_addr = context.tmp_val;

//     if (var_addr->get_type()->get_pointer_element_type() !=
//     expr_val->get_type()) {
//         if (expr_val->get_type()->is_float_type())
//             expr_val = builder->create_fptosi(expr_val, INT32_T);
//         else
//             expr_val = builder->create_sitofp(expr_val, FLOAT_T);
//     }
//     builder->create_store(expr_val, var_addr);

//     context.tmp_val = expr_val;
//     return nullptr;
// }

// Value* CminusfBuilder::visit(ASTSimpleExpression &node) {
//     // TODO: This function is empty now.
//     // Add some code here.
//     if (node.additive_expression_r == nullptr)
//         node.additive_expression_l->accept(*this);
//     else {
//         node.additive_expression_l->accept(*this);
//         Value *l_val = context.tmp_val;
//         node.additive_expression_r->accept(*this);
//         Value *r_val = context.tmp_val;

//         bool is_int = false;
//         if (l_val->get_type()->is_integer_type()) {
//             if (r_val->get_type()->is_integer_type())
//                 is_int = true;
//             else
//                 l_val = builder->create_sitofp(l_val, FLOAT_T);
//         }
//         else {
//             if (r_val->get_type()->is_integer_type())
//                 r_val = builder->create_sitofp(r_val, FLOAT_T);
//         }

//     Value *cmp;
//     switch (node.op) {
//         case OP_LT:
//             if (is_int)
//                 cmp = builder->create_icmp_lt(l_val, r_val);
//             else
//                 cmp = builder->create_fcmp_lt(l_val, r_val);
//             break;
//         case OP_LE:
//             if (is_int)
//                 cmp = builder->create_icmp_le(l_val, r_val);
//             else
//                 cmp = builder->create_fcmp_le(l_val, r_val);
//             break;
//         case OP_GE:
//             if (is_int)
//                 cmp = builder->create_icmp_ge(l_val, r_val);
//             else
//                 cmp = builder->create_fcmp_ge(l_val, r_val);
//             break;
//         case OP_GT:
//             if (is_int)
//                 cmp = builder->create_icmp_gt(l_val, r_val);
//             else
//                 cmp = builder->create_fcmp_gt(l_val, r_val);
//             break;
//         case OP_EQ:
//             if (is_int)
//                 cmp = builder->create_icmp_eq(l_val, r_val);
//             else
//                 cmp = builder->create_fcmp_eq(l_val, r_val);
//             break;
//         case OP_NEQ:
//             if (is_int)
//                 cmp = builder->create_icmp_ne(l_val, r_val);
//             else
//                 cmp = builder->create_fcmp_ne(l_val, r_val);
//             break;
//         }
//     // context.tmp_val = cmp;
//     context.tmp_val = builder->create_zext(cmp, INT32_T);
//     }
//     return nullptr;
// }

// Value* CminusfBuilder::visit(ASTAdditiveExpression &node) {
//     // TODO: This function is empty now.
//     // Add some code here.
//     if (node.additive_expression == nullptr)
//         node.term->accept(*this);
//     else {
//         node.additive_expression->accept(*this);
//         Value *l_val = context.tmp_val;
//         node.term->accept(*this);
//         Value *r_val = context.tmp_val;

//         bool is_int = false;
//         if (l_val->get_type()->is_integer_type()) {
//             if (r_val->get_type()->is_integer_type())
//                 is_int = true;
//             else
//                 l_val = builder->create_sitofp(l_val, FLOAT_T);
//         }
//         else {
//             if (r_val->get_type()->is_integer_type())
//                 r_val = builder->create_sitofp(r_val, FLOAT_T);
//         }

//         switch (node.op) {
//             case OP_PLUS:
//                 if (is_int)
//                     context.tmp_val = builder->create_iadd(l_val, r_val);
//                 else
//                     context.tmp_val = builder->create_fadd(l_val, r_val);
//                 break;
//             case OP_MINUS:
//                 if (is_int)
//                     context.tmp_val = builder->create_isub(l_val, r_val);
//                 else
//                     context.tmp_val = builder->create_fsub(l_val, r_val);
//                 break;
//         }
//     }
//     return nullptr;
// }

// Value* CminusfBuilder::visit(ASTTerm &node) {
//     // TODO: This function is empty now.
//     // Add some code here.
//     if (node.term == nullptr)
//         node.factor->accept(*this);
//     else {
//         node.term->accept(*this);
//         Value *l_val = context.tmp_val;
//         node.factor->accept(*this);
//         Value *r_val = context.tmp_val;

//         bool is_int = false;
//         if (l_val->get_type()->is_integer_type()) {
//             if (r_val->get_type()->is_integer_type())
//                 is_int = true;
//             else
//                 l_val = builder->create_sitofp(l_val, FLOAT_T);
//         }
//         else {
//             if (r_val->get_type()->is_integer_type())
//                 r_val = builder->create_sitofp(r_val, FLOAT_T);
//         }

//         switch (node.op) {
//             case OP_MUL:
//                 if (is_int)
//                     context.tmp_val = builder->create_imul(l_val, r_val);
//                 else
//                     context.tmp_val = builder->create_fmul(l_val, r_val);
//                 break;
//             case OP_DIV:
//                 if (is_int)
//                     context.tmp_val = builder->create_isdiv(l_val, r_val);
//                 else
//                     context.tmp_val = builder->create_fdiv(l_val, r_val);
//                 break;
//         }
//     }
//     return nullptr;
// }

// Value* CminusfBuilder::visit(ASTCall &node) {
//     // TODO: This function is empty now.
//     // Add some code here.
//     auto fun = static_cast<Function *>(scope.find(node.id));
//     std::vector<Value *> args;
//     auto param_type = fun->get_function_type()->param_begin();
//     for (auto &arg: node.args) {
//         arg->accept(*this);
//         if (!context.tmp_val->get_type()->is_pointer_type() &&
//             *param_type != context.tmp_val->get_type()) {
//             if (context.tmp_val->get_type()->is_integer_type())
//                 context.tmp_val = builder->create_sitofp(context.tmp_val,
//                 FLOAT_T);
//             else
//                 context.tmp_val = builder->create_fptosi(context.tmp_val,
//                 INT32_T);
//         }
//         args.push_back(context.tmp_val);
//         param_type++;
//     }

//     context.tmp_val = builder->create_call(fun, args);
//     return nullptr;
// }
