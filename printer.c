void print_type(Typespec *type);

#define INDENT_WIDTH 4
static int indent = 0; // for printing
                       //
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
        printf("(%c ", expr->unary.op); 
        print_expr(expr->unary.expr);
        printf(")");
        break;
    case EXPR_BINARY: 
        printf("(%c ", expr->binary.op); 
        print_expr(expr->binary.left);
        printf(" ");
        print_expr(expr->binary.right);
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
    case EXPR_COMPOUND: 
        printf("(");
        print_type(expr->compound.type);
        ++indent;
        for (int i=0; i<expr->compound.num_args; ++i) {
            printf("\n%*s", indent*INDENT_WIDTH, " ");
            print_expr(expr->compound.args[i]);
        }
        --indent;
        printf(")");
        break;
    default:
        fprintf(stderr, "Error: Printer: Unkown expr kind: %d\n", expr->kind);
        assert(0);
        break;
    }
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
        printf("{");
        printf("}");
        break;
    case STMT_EXPR:
        print_expr(stmt->expr);
        break;
    case STMT_IF:
        assert(0);
        /*printf("(if ");*/
        /*print_expr(stmt->if_stmt.cond);*/
        /*++indent;*/
        /*printf("\n%*s", indent*INDENT_WIDTH, " ");*/
        /*print_stmt_block(stmt->if_stmt.then_block);*/
        /*printf("\n%*s", indent*INDENT_WIDTH, " ");*/
        /*--indent;*/
        break;
    case STMT_FOR:
    case STMT_DO:
    case STMT_WHILE:
    case STMT_SWITCH:
    case STMT_ASSIGN:
    case STMT_INIT:
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
        printf(" (block");
        ++indent;
        StmtBlock block = decl->func.block;
        for (int i=0; i<block.num_stmts; ++i) {
            printf("\n%*s", indent*INDENT_WIDTH, " ");
            print_stmt(block.stmts[i]);
        }
        --indent;
        printf(")"); // close block
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
            printf("\n%*s", indent*INDENT_WIDTH, " ");
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
