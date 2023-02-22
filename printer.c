void print_type(Typespec *type);
void print_stmt(Stmt *stmt);

#define INDENT_WIDTH 4
static int indent = 0; // for printing

void print_newline(void) {
    if (indent > 0)
        printf("\n%*s", indent*INDENT_WIDTH, " ");
    else
        printf("\n");
}

void print_expr(Expr *expr) {
    if (expr == NULL) {
        printf("NULL");
        return;
    }

    switch (expr->kind) {
    case EXPR_INT: 
        printf("%d", expr->int_val);    
        break;
    case EXPR_FLOAT: 
        printf("%f", expr->float_val); 
        break;
    case EXPR_STR: 
        printf("\"%s\"", expr->str_val); 
        break;
    case EXPR_NAME: 
        printf("%s", expr->name); 
        break;
    case EXPR_UNARY: 
        if (expr->unary.op == '!')
            printf("(not ");
        else
            printf("(%c ", expr->unary.op); 
        print_expr(expr->unary.expr);
        printf(")");
        break;
    case EXPR_BINARY: 
        printf("(%s ", str_token_kind(expr->binary.op)); 
        print_expr(expr->binary.left);
        printf(" ");
        print_expr(expr->binary.right);
        printf(")");
        break;
    case EXPR_TERNARY: 
        printf("(ternary ");
        print_expr(expr->ternary.cond);
        printf(" ");
        print_expr(expr->ternary.then_expr);
        printf(" ");
        print_expr(expr->ternary.else_expr);
        printf(")");
        break;
    case EXPR_CALL: 
        printf("(%s", expr->call.expr->name);
        for (int i=0; i<expr->call.num_args; ++i) {
            printf(" ");
            print_expr(expr->call.args[i]);
        }
        printf(")");
        break;
    case EXPR_INDEX: 
        printf("(index ");
        print_expr(expr->index.expr);
        printf(" ");
        print_expr(expr->index.index);
        printf(")");
        break;
    case EXPR_FIELD: 
        printf("(field ");
        print_expr(expr->field.expr);
        printf(" %s)", expr->field.name);
        break;
    case EXPR_COMPOUND: 
        printf("(");
        print_type(expr->compound.type);
        ++indent;
        for (int i=0; i<expr->compound.num_args; ++i) {
            print_newline();
            print_expr(expr->compound.args[i]);
        }
        --indent;
        printf(")");
        break;
    case EXPR_OR:
        printf("(or ");
        print_expr(expr->or_expr.left);
        printf(" ");
        print_expr(expr->or_expr.right);
        printf(")");
        break;
    case EXPR_AND:
        printf("(and ");
        print_expr(expr->and_expr.left);
        printf(" ");
        print_expr(expr->and_expr.right);
        printf(")");
        break;
    case EXPR_CMP:
        printf("(%s ", str_token_kind(expr->cmp.op)); 
        print_expr(expr->cmp.left);
        printf(" ");
        print_expr(expr->cmp.right);
        printf(")");
        break;
    case EXPR_CAST:
        printf("(cast ");
        print_type(expr->cast.type);
        printf(" ");
        print_expr(expr->cast.expr);
        printf(")");
        break;
    default:
        fprintf(stderr, "Error: Printer: Unkown expr kind: %d\n", expr->kind);
        assert(0);
        break;
    }
}

void print_stmt_block(StmtBlock block) {
    printf("(block");
    ++indent;
    for (int i=0; i<block.num_stmts; ++i) {
        print_newline();
        print_stmt(block.stmts[i]);
    }
    --indent;
    printf(")");
}

void print_stmt(Stmt *stmt) {
    if (stmt == NULL) {
        printf("NULL");
        return;
    }

    switch (stmt->kind) {
    case STMT_RETURN:
        printf("(return");
        if (stmt->return_stmt.expr) {
            printf(" ");
            print_expr(stmt->return_stmt.expr);
        }
        printf(")");
        break;
    case STMT_CONTINUE:
        printf("continue");
        break;
    case STMT_BREAK:
        printf("break");
        break;
    case STMT_BRACE_BLOCK:
        print_stmt_block(stmt->block);
        break;
    case STMT_EXPR:
        print_expr(stmt->expr);
        break;
    case STMT_IF:
        printf("(if ");
        print_expr(stmt->if_stmt.cond);
        ++indent;
        print_newline();
        print_stmt_block(stmt->if_stmt.then_block);
        --indent;
        for (int i=0; i<stmt->if_stmt.num_else_ifs; ++i) {
            ElseIf else_if = stmt->if_stmt.else_ifs[i];
            print_newline();
            printf("(elseif ");
            print_expr(else_if.cond);
            ++indent;
            print_newline();
            print_stmt_block(else_if.block);
            --indent;
        }
        if (stmt->if_stmt.else_block.num_stmts > 0) {
            print_newline();
            print_stmt_block(stmt->if_stmt.else_block);
        }
        printf(")");
        break;
    case STMT_INIT:
        printf("(auto-assign %s ", stmt->init.name);
        print_expr(stmt->init.expr);
        printf(")");
        break;
    case STMT_ASSIGN:
        printf("(%s ", str_token_kind(stmt->assign.op));
        print_expr(stmt->assign.left);
        if (stmt->assign.right) {
            printf(" ");
            print_expr(stmt->assign.right);
        }
        printf(")");
        break;
    case STMT_FOR: 
        printf("(for ");
        print_stmt(stmt->for_stmt.init);
        printf(" ");
        print_expr(stmt->for_stmt.cond);
        printf(" ");
        print_stmt(stmt->for_stmt.next);
        printf(" ");
        print_stmt_block(stmt->for_stmt.block);
        printf(")");
        break;
    case STMT_DO:
        printf("(do ");
        print_stmt_block(stmt->while_stmt.block);
        print_newline();
        printf("(while ");
        print_expr(stmt->while_stmt.cond);
        printf(")");
        printf(")");
        break;
    case STMT_WHILE:
        printf("(while ");
        print_expr(stmt->while_stmt.cond);
        printf(" ");
        print_stmt_block(stmt->while_stmt.block);
        printf(")");
        break;
    case STMT_SWITCH:
        printf("(switch ");
        print_expr(stmt->switch_stmt.expr);
        ++indent;
        for (int i=0; i<stmt->switch_stmt.num_cases; ++i) {
            SwitchCase *sc = &stmt->switch_stmt.cases[i];
            print_newline();
            if (sc->is_default) 
                printf("(default");
            else
                printf("(case");
            for (int j=0; j<sc->num_exprs; ++j) {
                printf(" ");
                print_expr(sc->exprs[j]);
            }
            printf(" ");
            print_stmt_block(sc->block);
            printf(")");
        }
        --indent;
        printf(")");
        break;
    default:
        assert(0);
        break;
    }
}

void print_type(Typespec *type) {
    if (type == NULL) {
        printf("NULL");
        return;
    }

    switch (type->kind) {
    case TYPESPEC_NAME:
        printf("%s", type->name);
        break;
    case TYPESPEC_POINTER:
        printf("(ptr ");
        print_type(type->ptr.elem);
        printf(")");
        break;
    case TYPESPEC_ARRAY:
        printf("(array ");
        print_expr(type->array.size);
        printf(" ");
        print_type(type->array.elem);
        printf(")");
        break;
    default:
        assert(0);
        break;
    }
}

void print_decl(Decl *decl) {
    switch (decl->kind) {
    case DECL_CONST:
        printf("(const %s ", decl->name);
        print_expr(decl->const_decl.expr);
        printf(")");
        break;
    case DECL_VAR:
        printf("(var %s ", decl->name);
        print_type(decl->var.type);
        printf(" ");
        print_expr(decl->var.expr);
        printf(")");
        break;
    case DECL_TYPEDEF:
        printf("(typedef %s ", decl->name);
        print_type(decl->typedef_decl.type);
        printf(")");
        break;
    case DECL_FUNC:
        printf("(func %s (", decl->name);
        for (int i=0; i<decl->func.num_params; ++i) {
            FuncParam p = decl->func.params[i];
            if (i > 0) printf(" ");
            printf("%s ", p.name);
            print_type(p.type);
        }
        printf(")");
        if (decl->func.ret_type) {
            printf(" ");
            print_type(decl->func.ret_type);
        }
        printf(" ");
        print_stmt_block(decl->func.block);
        printf(")"); // close func
        break;
    case DECL_UNION:
    case DECL_STRUCT:
        if (decl->kind == DECL_STRUCT) {
            printf("(struct %s ", decl->name);
        } else {
            assert(decl->kind == DECL_UNION);
            printf("(union %s ", decl->name);
        }

        ++indent;
        for (int i=0; i<decl->aggregate.num_fields; ++i) {
            print_newline();
            AggregateField f = decl->aggregate.fields[i];
            printf("(%s ", f.name);
            print_type(f.type);
            printf(")");
        }
        --indent;
        printf(")");
        break;
    default:
        assert(0 && "Unknown decl kind");
        break;
    }
}
