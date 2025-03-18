#include <vector>

// Building
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/Host.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/CodeGen/TargetPassConfig.h>

// main llvm api
#include <llvm/IR/Instructions.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/ADT/APInt.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <unordered_map>

#include "codegen.hxx"
#include "types.hxx"
#include "err.hxx"
#include "utils.hxx"
#include "ds/array.hxx"
#include "ds/umap.hxx"

typedef struct {
        Token *id;
        Scr_Type rty;
        llvm::Function *value;
} Proc;

typedef struct {
        Token *id;
        Scr_Type *ty;
        llvm::Value *value;
} Var;

typedef struct {
        Array<Umap<char *, Var *> *> vs;
        Array<Umap<char *, Proc *> *> ps;
        Array<Umap<char *, Stmt_Struct *>> ss;
        llvm::LLVMContext *llctx;
        llvm::IRBuilder<> *bl;
        llvm::Module *md;
} Context;

static llvm::Value *compile_stmt(Stmt *s, Context *ctx);
static llvm::Value *compile_expr(Expr *e, Context *ctx);

static bool proc_in_scope(char *id, Context *ctx) {
        for (int i = (int)ctx->ps.length()-1; i >= 0; --i) {
                if (ctx->ps[i]->has(id)) {
                        return true;
                }
        }
        return false;
}

static bool var_in_scope(char *id, Context *ctx) {
        for (int i = (int)ctx->vs.length()-1; i >= 0; --i) {
                if (ctx->vs[i]->has(id)) {
                        return true;
                }
        }
        return false;
}

static void assert_identifier_in_scope(char *id, Context *ctx) {
        if (!var_in_scope(id, ctx)) {
                if (!proc_in_scope(id, ctx)) {
                        err_wargs("identifier %s is not declared", id);
                }
        }
}

static void assert_identifier_not_in_scope(char *id, Context *ctx) {
        if (var_in_scope(id, ctx) || proc_in_scope(id, ctx)) {
                err_wargs("identifier %s is already declared", id);
        }
}

static void assert_var_in_scope(char *id, Context *ctx) {
        if (!var_in_scope(id, ctx)) {
                err_wargs("identifier %s is not declared", id);
        }
}

static void assert_var_not_in_scope(char *id, Context *ctx) {
        if (var_in_scope(id, ctx)) {
                err_wargs("identifier %s is already declared", id);
        }
}

static void add_var_to_scope(Var *v, Context *ctx) {
        // ctx->vs.back().add(v->id->lx, v);
        ctx->vs[ctx->vs.length()-1]->add(v->id->lx, v);
}

static void push_scope(Context *ctx) {
        auto m = new Umap<char *, Var *>([](char *s0, char *s1) {
                return !strcmp(s0, s1);
        });
        ctx->vs.add(m);
}

static void pop_scope(Context *ctx) {
        if (ctx->vs.length() == 0) {
                err("cannot pop scope, out of length");
        }
        ctx->vs.pop_back();
}

static llvm::Type *scr_type_to_llvm_type(Scr_Type *ty, Context *ctx) {
        switch (ty->base) {
        case SCR_BASE_TYPE_I8:
                return llvm::Type::getInt8Ty(*(ctx->llctx));
        case SCR_BASE_TYPE_I16:
                return llvm::Type::getInt16Ty(*(ctx->llctx));
        case SCR_BASE_TYPE_I32:
                return llvm::Type::getInt32Ty(*(ctx->llctx));
        case SCR_BASE_TYPE_I64:
                return llvm::Type::getInt64Ty(*(ctx->llctx));
        case SCR_BASE_TYPE_U8:
                return llvm::Type::getInt8Ty(*(ctx->llctx));  // LLVM treats u8 as i8 (unsigned is semantic)
        case SCR_BASE_TYPE_U16:
                return llvm::Type::getInt16Ty(*(ctx->llctx)); // u16 as i16
        case SCR_BASE_TYPE_U32:
                return llvm::Type::getInt32Ty(*(ctx->llctx)); // u32 as i32
        case SCR_BASE_TYPE_U64:
                return llvm::Type::getInt64Ty(*(ctx->llctx)); // u64 as i64
        case SCR_BASE_TYPE_STR:
                return llvm::Type::getInt8PtrTy(*(ctx->llctx)); // str as i8*
        case SCR_BASE_TYPE_VOID:
                return llvm::Type::getVoidTy(*(ctx->llctx));
        case SCR_BASE_TYPE_PTR: {
                if (!ty->ptrn) {
                        err("pointer type missing pointee type");
                        return nullptr;
                }
                // Recursively get the type being pointed to and create a pointer to it
                llvm::Type *pointee_type = scr_type_to_llvm_type(ty->ptrn, ctx);
                if (!pointee_type) {
                        err("failed to resolve pointee type for pointer");
                        return nullptr;
                }
                return llvm::PointerType::get(pointee_type, 0); // 0 is the address space
        }
        case SCR_BASE_TYPE_CUSTOM: {
                if (!ty->custom_name) {
                        err("custom type missing name");
                        return nullptr;
                }
                // Look up the struct in the context
                Stmt_Struct *struct_def = nullptr;
                for (int i = (int)ctx->ss.length() - 1; i >= 0; --i) {
                        if (ctx->ss[i].has(ty->custom_name)) {
                                struct_def = *ctx->ss[i].get(ty->custom_name);  // Get the struct definition
                                break;
                        }
                }
                if (!struct_def) {
                        err_wargs("undefined struct type '%s'", ty->custom_name);
                        return nullptr;
                }

                // Check if the struct type already exists in LLVM
                llvm::StructType *struct_type = llvm::StructType::getTypeByName(*(ctx->llctx), ty->custom_name);
                if (!struct_type) {
                        // If it doesn't exist, it should have been created in compile_stmt_struct
                        // This assumes compile_stmt_struct has already processed the struct definition
                        err_wargs("struct type '%s' not found in LLVM context; ensure it’s defined before use", ty->custom_name);
                        return nullptr;
                }

                return struct_type;
        }
        default: {
                err_wargs("unhandled type %d", (int)ty->base);
        }
        }
        return nullptr; // unreachable
}

static llvm::Function *gen_proc_proto(Stmt_Proc *s, Context *ctx) {
        std::vector<llvm::Type *> types;

        for (size_t i = 0; i < s->args.len; ++i) {
                llvm::Type *llty = scr_type_to_llvm_type(s->args.types[i], ctx);
                types.push_back(llty);
        }

        // If the function is variadic, set the last argument type to be a pointer type (e.g., void*)
        llvm::FunctionType *ft = llvm::FunctionType::get(scr_type_to_llvm_type(s->rtype, ctx),
                                                         types, s->variadic);

        llvm::Function *f = llvm::Function::Create(
                ft, llvm::Function::ExternalLinkage, s->id->lx, ctx->md);

        size_t idx = 0;
        for (auto &a : f->args()) {
                a.setName(s->args.ids[idx++]->lx);
        }

        return f;
}

static llvm::Value *compile_expr_int_lit(Expr_Int_Lit *e, Context *ctx) {
        return llvm::ConstantInt::get(*(ctx->llctx),
                                      llvm::APInt(/*bits=*/32, (uint32_t)e->i));
}

static llvm::Value *compile_expr_proc_call(Expr_Proc_Call *e, Context *ctx) {
        llvm::Value *callee = compile_expr(e->left, ctx);
        if (!callee) {
                err("failed to compile function expression in procedure call");
        }

        std::vector<llvm::Value *> args;
        for (size_t i = 0; i < e->args.len; ++i) {
                llvm::Value *arg = compile_expr(e->args.exprs[i], ctx);
                if (!arg) {
                        err_wargs("failed to compile argument %zu in procedure call", i);
                }
                // If the argument is a pointer to a variable, load its value
                if (arg->getType()->isPointerTy() && e->args.exprs[i]->ty == EXPR_TYPE_IDENT) {
                        char *id = ((Expr_Ident *)e->args.exprs[i])->id->lx;
                        for (int j = (int)ctx->vs.length() - 1; j >= 0; --j) {
                                if (ctx->vs[j]->has(id)) {
                                        Var *var = *(ctx->vs[j]->get(id));
                                        arg = ctx->bl->CreateLoad(scr_type_to_llvm_type(var->ty, ctx), arg, id);
                                        break;
                                }
                        }
                }
                args.push_back(arg);
        }

        llvm::Function *func = llvm::dyn_cast<llvm::Function>(callee);
        if (!func) {
                err("callee is not a directly callable function");
        }

        return ctx->bl->CreateCall(func, args);
}

static llvm::Value *compile_expr_ident(Expr_Ident *e, Context *ctx) {
        char *id = e->id->lx;

        // Check variables in scope
        for (int i = (int)ctx->vs.length() - 1; i >= 0; --i) {
                if (ctx->vs[i]->has(id)) {
                        Var *var = *(ctx->vs[i]->get(id));
                        // Return the pointer (e.g., AllocaInst*) directly, not the loaded value
                        return var->value;
                }
        }

        // Check procedures in scope
        for (int i = (int)ctx->ps.length() - 1; i >= 0; --i) {
                if (ctx->ps[i]->has(id)) {
                        Proc *proc = *(ctx->ps[i]->get(id));
                        return proc->value;  // Function pointer
                }
        }

        // Check module-level functions
        if (llvm::Function *func = ctx->md->getFunction(id)) {
                return func;
        }

        err_wargs("undefined identifier '%s'", id);
        return nullptr;
}

static llvm::Value *compile_expr_str_lit(Expr_Str_Lit *e, Context *ctx) {
        const char *str = e->s->lx;

        // Create a global constant string
        llvm::Constant *str_constant = llvm::ConstantDataArray::getString(
                *ctx->llctx, str,
                true  /* Add null terminator */);

        // Create a global variable to hold the string
        llvm::GlobalVariable *gv = new llvm::GlobalVariable(
                *ctx->md,                          // Module
                str_constant->getType(),            // Type of the string array
                true,                              // isConstant
                llvm::GlobalValue::PrivateLinkage, // Linkage
                str_constant,                       // Initializer
                ""                                 // Name
                );

        // Get a pointer to the start of the string (i8*)
        llvm::Constant *zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*ctx->llctx), 0);
        std::vector<llvm::Constant*> indices = {zero, zero};
        llvm::Constant *str_ptr = llvm::ConstantExpr::getGetElementPtr(str_constant->getType(), gv, indices);

        return str_ptr;
}

static llvm::Value *compile_expr_mut(Expr_Mut *e, Context *ctx) {
        llvm::Value *left = nullptr;
        llvm::Type *val_type = nullptr; // Type of the value to load/store

        if (e->l->ty == EXPR_TYPE_IDENT) {
                left = compile_expr_ident((Expr_Ident *)e->l, ctx);
                if (!left) {
                        err("failed to compile left operand (identifier) of assignment");
                        return nullptr;
                }
                // Get the type from the variable scope
                char *id = ((Expr_Ident *)e->l)->id->lx;
                for (int i = (int)ctx->vs.length() - 1; i >= 0; --i) {
                        if (ctx->vs[i]->has(id)) {
                                Var *var = *(ctx->vs[i]->get(id));
                                val_type = scr_type_to_llvm_type(var->ty, ctx);
                                break;
                        }
                }
                if (!val_type) {
                        err_wargs("could not determine type of identifier '%s'", id);
                        return nullptr;
                }
        } else if (e->l->ty == EXPR_TYPE_GET) {
                Expr_Get *get_expr = (Expr_Get *)e->l;
                llvm::Value *base_ptr = compile_expr(get_expr->l, ctx);
                if (!base_ptr || !base_ptr->getType()->isPointerTy()) {
                        err("base of field access must be a pointer");
                        return nullptr;
                }
                if (get_expr->l->ty != EXPR_TYPE_IDENT) {
                        err("base of field access must be an identifier for now");
                        return nullptr;
                }
                Expr_Ident *base_ident = (Expr_Ident *)get_expr->l;
                char *base_id = base_ident->id->lx;

                Var *var = nullptr;
                for (int i = (int)ctx->vs.length() - 1; i >= 0; --i) {
                        if (ctx->vs[i]->has(base_id)) {
                                var = *(ctx->vs[i]->get(base_id));
                                break;
                        }
                }
                if (!var) {
                        err_wargs("variable '%s' not found in scope", base_id);
                        return nullptr;
                }

                llvm::Type *base_type = scr_type_to_llvm_type(var->ty, ctx);
                if (!base_type->isStructTy()) {
                        err_wargs("variable '%s' must be a struct for field access", base_id);
                        return nullptr;
                }
                llvm::StructType *struct_type = llvm::cast<llvm::StructType>(base_type);

                if (get_expr->r->ty != EXPR_TYPE_IDENT) {
                        err("field must be an identifier");
                        return nullptr;
                }
                Expr_Ident *field_ident = (Expr_Ident *)get_expr->r;
                char *field_name = field_ident->id->lx;

                Stmt_Struct *struct_def = nullptr;
                const std::string struct_name = struct_type->getName().str();
                for (int i = (int)ctx->ss.length() - 1; i >= 0; --i) {
                        if (ctx->ss[i].has((char *)struct_name.c_str())) {
                                struct_def = *ctx->ss[i].get((char *)struct_name.c_str());
                                break;
                        }
                }
                if (!struct_def) {
                        err_wargs("struct type '%s' not found", struct_name.c_str());
                        return nullptr;
                }

                unsigned field_index = UINT_MAX;
                for (size_t i = 0; i < struct_def->fields.len; ++i) {
                        if (!strcmp(struct_def->fields.ids[i]->lx, field_name)) {
                                field_index = i;
                                break;
                        }
                }
                if (field_index == UINT_MAX) {
                        err_wargs("field '%s' not found in struct '%s'", field_name, struct_name.c_str());
                        return nullptr;
                }

                std::vector<llvm::Value *> indices;
                indices.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(*ctx->llctx), 0));
                indices.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(*ctx->llctx), field_index));
                left = ctx->bl->CreateGEP(struct_type, base_ptr, indices, "fieldptr");

                // Get the field type from the struct definition
                val_type = scr_type_to_llvm_type(struct_def->fields.types[field_index], ctx);
        } else {
                left = compile_expr(e->l, ctx);
                if (!left) {
                        err("failed to compile left operand of assignment");
                        return nullptr;
                }
                // Fallback: assume left’s type provides the value type (may fail for unsupported cases)
                if (left->getType()->isPointerTy()) {
                        err("type inference for non-identifier, non-get lvalue not supported yet");
                        return nullptr;
                }
        }

        if (!left) {
                err("failed to compile left operand of assignment");
                return nullptr;
        }
        if (!left->getType()->isPointerTy()) {
                err("left operand of assignment must be an lvalue");
                return nullptr;
        }
        if (!val_type) {
                err("could not determine value type for assignment");
                return nullptr;
        }

        llvm::Value *right = compile_expr(e->r, ctx);
        if (!right) {
                err("failed to compile right operand of assignment");
                return nullptr;
        }

        const char *op = e->op->lx;
        if (!strcmp(op, "=")) {
                ctx->bl->CreateStore(right, left);
                return right;
        } else {
                // Compound assignments
                llvm::Value *current = ctx->bl->CreateLoad(val_type, left, "loadtmp");

                llvm::Value *result = nullptr;
                if (!strcmp(op, "+=")) {
                        result = ctx->bl->CreateAdd(current, right, "addtmp");
                } else if (!strcmp(op, "-=")) {
                        result = ctx->bl->CreateSub(current, right, "subtmp");
                } else if (!strcmp(op, "*=")) {
                        result = ctx->bl->CreateMul(current, right, "multmp");
                } else if (!strcmp(op, "/=")) {
                        result = ctx->bl->CreateSDiv(current, right, "divtmp");
                } else {
                        err_wargs("unsupported assignment operator '%s'", op);
                        return nullptr;
                }

                ctx->bl->CreateStore(result, left);
                return result;
        }

        assert(0 && "unreachable");
}

static llvm::Value *compile_expr_bin(Expr_Bin *e, Context *ctx) {
        llvm::Value *left = compile_expr(e->l, ctx);
        if (!left) {
                err("failed to compile left operand of binary expression");
        }
        // Load if it’s a pointer to a variable
        if (left->getType()->isPointerTy() && e->l->ty == EXPR_TYPE_IDENT) {
                char *id = ((Expr_Ident *)e->l)->id->lx;
                for (int i = (int)ctx->vs.length() - 1; i >= 0; --i) {
                        if (ctx->vs[i]->has(id)) {
                                Var *var = *(ctx->vs[i]->get(id));
                                left = ctx->bl->CreateLoad(scr_type_to_llvm_type(var->ty, ctx), left, id);
                                break;
                        }
                }
        }

        llvm::Value *right = compile_expr(e->r, ctx);
        if (!right) {
                err("failed to compile right operand of binary expression");
        }
        // Load if it’s a pointer to a variable
        if (right->getType()->isPointerTy() && e->r->ty == EXPR_TYPE_IDENT) {
                char *id = ((Expr_Ident *)e->r)->id->lx;
                for (int i = (int)ctx->vs.length() - 1; i >= 0; --i) {
                        if (ctx->vs[i]->has(id)) {
                                Var *var = *(ctx->vs[i]->get(id));
                                right = ctx->bl->CreateLoad(scr_type_to_llvm_type(var->ty, ctx), right, id);
                                break;
                        }
                }
        }

        const char *op = e->op->lx;
        llvm::Type *i32Ty = llvm::Type::getInt32Ty(*(ctx->llctx));
        llvm::Type *i1Ty = llvm::Type::getInt1Ty(*(ctx->llctx));

        // Arithmetic operations
        if (!strcmp(op, "+") || !strcmp(op, "-") || !strcmp(op, "*") ||
            !strcmp(op, "/") || !strcmp(op, "%")) {
                if (left->getType() != i32Ty || right->getType() != i32Ty) {
                        err_wargs("binary operator '%s' requires i32 operands", op);
                }
                if (!strcmp(op, "+")) {
                        return ctx->bl->CreateAdd(left, right, "addtmp");
                } else if (!strcmp(op, "-")) {
                        return ctx->bl->CreateSub(left, right, "subtmp");
                } else if (!strcmp(op, "*")) {
                        return ctx->bl->CreateMul(left, right, "multmp");
                } else if (!strcmp(op, "/")) {
                        return ctx->bl->CreateSDiv(left, right, "sdivtmp");
                } else if (!strcmp(op, "%")) {
                        return ctx->bl->CreateSRem(left, right, "remtmp");
                }
        }
        // Comparison operations
        else if (!strcmp(op, "==") || !strcmp(op, "!=") || !strcmp(op, ">=") ||
                 !strcmp(op, "<=") || !strcmp(op, ">") || !strcmp(op, "<")) {
                if (left->getType() != i32Ty || right->getType() != i32Ty) {
                        err_wargs("comparison operator '%s' requires i32 operands", op);
                }
                if (!strcmp(op, "==")) {
                        return ctx->bl->CreateICmpEQ(left, right, "eqtmp");
                } else if (!strcmp(op, "!=")) {
                        return ctx->bl->CreateICmpNE(left, right, "netmp");
                } else if (!strcmp(op, ">=")) {
                        return ctx->bl->CreateICmpSGE(left, right, "sgetmp");
                } else if (!strcmp(op, "<=")) {
                        return ctx->bl->CreateICmpSLE(left, right, "sletmp");
                } else if (!strcmp(op, ">")) {
                        return ctx->bl->CreateICmpSGT(left, right, "sgttmp");
                } else if (!strcmp(op, "<")) {
                        return ctx->bl->CreateICmpSLT(left, right, "slttmp");
                }
        }
        // Logical operations
        else if (!strcmp(op, "&&") || !strcmp(op, "||")) {
                if (left->getType() != i1Ty || right->getType() != i1Ty) {
                        err_wargs("logical operator '%s' requires i1 operands", op);
                }
                if (!strcmp(op, "&&")) {
                        return ctx->bl->CreateAnd(left, right, "andtmp");
                } else if (!strcmp(op, "||")) {
                        return ctx->bl->CreateOr(left, right, "ortmp");
                }
        } else {
                err_wargs("unsupported binary operator '%s'", op);
        }
        return nullptr; // unreachable
}

static llvm::Value *compile_expr_expr_struct_inst(Expr_Struct_Inst *s, Context *ctx) {
        // Step 1: Find the struct definition in the context
        char *struct_name = s->struct_name->lx;
        Stmt_Struct *struct_def = nullptr;

        for (int i = (int)ctx->ss.length() - 1; i >= 0; --i) {
                if (ctx->ss[i].has(struct_name)) {
                        struct_def = *ctx->ss[i].get(struct_name);
                        break;
                }
        }

        if (!struct_def) {
                err_wargs("undefined struct type '%s'", struct_name);
                return nullptr;
        }

        // Step 2: Get the LLVM struct type
        llvm::StructType *struct_type = llvm::StructType::getTypeByName(*ctx->llctx, struct_name);
        if (!struct_type) {
                // If not found, create it based on the struct definition
                std::vector<llvm::Type *> field_types;
                for (size_t i = 0; i < struct_def->fields.len; ++i) {
                        field_types.push_back(scr_type_to_llvm_type(struct_def->fields.types[i], ctx));
                }
                struct_type = llvm::StructType::create(*ctx->llctx, field_types, struct_name);
        }

        // Step 3: Compile the field initializers and validate them
        std::vector<llvm::Value *> field_values(struct_def->fields.len, nullptr);
        std::vector<bool> field_initialized(struct_def->fields.len, false);

        for (size_t i = 0; i < s->len; ++i) {
                char *field_name = s->ids[i]->lx;
                llvm::Value *field_value = compile_expr(s->exprs[i], ctx);
                if (!field_value) {
                        err_wargs("failed to compile initializer for field '%s'", field_name);
                        return nullptr;
                }

                // Find the field index in the struct definition
                size_t field_idx = SIZE_MAX;
                for (size_t j = 0; j < struct_def->fields.len; ++j) {
                        if (!strcmp(struct_def->fields.ids[j]->lx, field_name)) {
                                field_idx = j;
                                break;
                        }
                }

                if (field_idx == SIZE_MAX) {
                        err_wargs("field '%s' does not exist in struct '%s'", field_name, struct_name);
                        return nullptr;
                }

                // Check if the field was already initialized
                if (field_initialized[field_idx]) {
                        err_wargs("field '%s' initialized multiple times in struct '%s'", field_name, struct_name);
                        return nullptr;
                }

                // Verify the type matches
                llvm::Type *expected_type = scr_type_to_llvm_type(struct_def->fields.types[field_idx], ctx);
                if (field_value->getType() != expected_type) {
                        err_wargs("type mismatch for field '%s' in struct '%s'", field_name, struct_name);
                        return nullptr;
                }

                field_values[field_idx] = field_value;
                field_initialized[field_idx] = true;
        }

        // Step 4: Fill in default values (e.g., 0) for uninitialized fields
        for (size_t i = 0; i < struct_def->fields.len; ++i) {
                if (!field_initialized[i]) {
                        llvm::Type *field_type = scr_type_to_llvm_type(struct_def->fields.types[i], ctx);
                        if (field_type->isIntegerTy()) {
                                field_values[i] = llvm::ConstantInt::get(field_type, 0);
                        } else {
                                err_wargs("uninitialized field '%s' in struct '%s' requires explicit initialization",
                                          struct_def->fields.ids[i]->lx, struct_name);
                                return nullptr;
                        }
                }
        }

        // Step 5: Create the struct value
        // Since this is an expression, we create a constant struct if all values are constant,
        // otherwise we need to insert it into the IR
        bool all_constant = true;
        for (llvm::Value *val : field_values) {
                if (!llvm::isa<llvm::Constant>(val)) {
                        all_constant = false;
                        break;
                }
        }

        if (all_constant) {
                std::vector<llvm::Constant *> constant_values;
                for (llvm::Value *val : field_values) {
                        constant_values.push_back(llvm::cast<llvm::Constant>(val));
                }
                return llvm::ConstantStruct::get(struct_type, constant_values);
        } else {
                // Create an alloca to hold the struct, then store each field
                llvm::AllocaInst *struct_alloca = ctx->bl->CreateAlloca(struct_type, nullptr, "structtmp");

                for (size_t i = 0; i < field_values.size(); ++i) {
                        std::vector<llvm::Value *> indices;
                        indices.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(*ctx->llctx), 0)); // First element of GEP
                        indices.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(*ctx->llctx), i));  // Field index
                        llvm::Value *field_ptr = ctx->bl->CreateGEP(struct_type, struct_alloca, indices, "fieldptr");
                        ctx->bl->CreateStore(field_values[i], field_ptr);
                }

                // Return the pointer to the struct (will be stored by the caller, e.g., in a let statement)
                return struct_alloca;
        }

        assert(0 && "unreachable");
        return nullptr;
}

static llvm::Value *compile_expr_get(Expr_Get *e, Context *ctx) {
        llvm::Value *base = compile_expr(e->l, ctx); // p
        if (!base) {
                err("compile_expr_get: failed to compile base expression");
                return nullptr;
        }

        // Ensure base is an identifier (for now)
        if (e->l->ty != EXPR_TYPE_IDENT) {
                err("compile_expr_get: left-hand side must be an identifier for now");
                return nullptr;
        }
        Expr_Ident *base_ident = (Expr_Ident *)e->l;
        char *base_id = base_ident->id->lx;

        // Look up the variable in scope to get its type
        Var *var = nullptr;
        for (int i = (int)ctx->vs.length() - 1; i >= 0; --i) {
                if (ctx->vs[i]->has(base_id)) {
                        var = *(ctx->vs[i]->get(base_id));
                        break;
                }
        }
        if (!var) {
                err_wargs("compile_expr_get: variable '%s' not found in scope", base_id);
                return nullptr;
        }

        llvm::Type *base_type = scr_type_to_llvm_type(var->ty, ctx);
        if (!base_type->isStructTy()) {
                err_wargs("compile_expr_get: variable '%s' must be a struct", base_id);
                return nullptr;
        }
        llvm::StructType *struct_type = llvm::cast<llvm::StructType>(base_type);

        // Get the field name
        if (e->r->ty != EXPR_TYPE_IDENT) {
                err("compile_expr_get: right-hand side must be an identifier");
                return nullptr;
        }
        Expr_Ident *field_ident = (Expr_Ident *)e->r;
        char *field_name = field_ident->id->lx;

        // Find the field index
        Stmt_Struct *struct_def = nullptr;
        const std::string struct_name = struct_type->getName().str();
        for (int i = (int)ctx->ss.length() - 1; i >= 0; --i) {
                if (ctx->ss[i].has((char *)struct_name.c_str())) {
                        struct_def = *ctx->ss[i].get((char *)struct_name.c_str());
                        break;
                }
        }
        if (!struct_def) {
                err_wargs("compile_expr_get: struct type '%s' not found in context", struct_name.c_str());
                return nullptr;
        }

        unsigned field_index = UINT_MAX;
        for (size_t i = 0; i < struct_def->fields.len; ++i) {
                if (!strcmp(struct_def->fields.ids[i]->lx, field_name)) {
                        field_index = i;
                        break;
                }
        }
        if (field_index == UINT_MAX) {
                err_wargs("compile_expr_get: field '%s' not found in struct '%s'", field_name, struct_name.c_str());
                return nullptr;
        }

        // Handle pointer (local var) or value (parameter)
        if (base->getType()->isPointerTy()) {
                // Local variable: use GEP and load
                std::vector<llvm::Value *> indices;
                indices.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(*ctx->llctx), 0));
                indices.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(*ctx->llctx), field_index));
                llvm::Value *field_ptr = ctx->bl->CreateGEP(struct_type, base, indices, "fieldptr");
                llvm::Type *field_type = scr_type_to_llvm_type(struct_def->fields.types[field_index], ctx);
                return ctx->bl->CreateLoad(field_type, field_ptr, field_name);
        } else if (base->getType()->isStructTy()) {
                // Parameter: extract value directly
                return ctx->bl->CreateExtractValue(base, field_index, field_name);
        } else {
                err("compile_expr_get: base must be a pointer to struct or struct value");
                return nullptr;
        }
}

static llvm::Value *compile_expr(Expr *e, Context *ctx) {
        switch (e->ty) {
        case EXPR_TYPE_UNARY: {
                assert(0 && "todo");
        } break;
        case EXPR_TYPE_BIN: {
                return compile_expr_bin((Expr_Bin *)e, ctx);
        } break;
        case EXPR_TYPE_MUT: {
                return compile_expr_mut((Expr_Mut *)e, ctx);
        } break;
        case EXPR_TYPE_IDENT: {
                return compile_expr_ident((Expr_Ident *)e, ctx); // Just return the pointer
        } break;
        case EXPR_TYPE_STR_LIT: {
                return compile_expr_str_lit((Expr_Str_Lit *)e, ctx);
        } break;
        case EXPR_TYPE_INT_LIT: {
                return compile_expr_int_lit((Expr_Int_Lit *)e, ctx);
        } break;
        case EXPR_TYPE_PROC_CALL: {
                return compile_expr_proc_call((Expr_Proc_Call *)e, ctx);
        } break;
        case EXPR_TYPE_STRUCT_INST: {
                return compile_expr_expr_struct_inst((Expr_Struct_Inst *)e, ctx);
        } break;
        case EXPR_TYPE_GET: {
                return compile_expr_get((Expr_Get *)e, ctx);
        } break;
        default: {
                err_wargs("unknown expression type %d", (int)e->ty);
        } break;
        }
        assert(0 && "unreachable");
        return nullptr;
}

static llvm::Value *compile_stmt_block(Stmt_Block *s, Context *ctx) {
        llvm::Value *res = nullptr;
        push_scope(ctx);
        for (size_t i = 0; i < s->len; ++i) {
                res = compile_stmt(s->stmts[i], ctx);
        }
        pop_scope(ctx);
        return res;
}

static llvm::Function *compile_stmt_proc(Stmt_Proc *s, Context *ctx) {
        llvm::Function *existing_function = ctx->md->getFunction(s->id->lx);
        if (!existing_function) {
                existing_function = gen_proc_proto(s, ctx);
        }
        if (!existing_function) {
                return nullptr;
        }
        if (!existing_function->empty()) {
                err_wargs("function %s cannot be redefined", s->id->lx);
        }

        llvm::BasicBlock *bb = llvm::BasicBlock::Create(*(ctx->llctx), "entry", existing_function);
        ctx->bl->SetInsertPoint(bb);

        push_scope(ctx);

        // Allocate and store function arguments
        for (auto &arg : existing_function->args()) {
                size_t arg_no = arg.getArgNo();
                llvm::Type *arg_type = scr_type_to_llvm_type(s->args.types[arg_no], ctx);
                llvm::AllocaInst *arg_alloca = ctx->bl->CreateAlloca(arg_type, nullptr, s->args.ids[arg_no]->lx);
                ctx->bl->CreateStore(&arg, arg_alloca);
                Var *v = new Var{ s->args.ids[arg_no], s->args.types[arg_no], arg_alloca };
                add_var_to_scope(v, ctx);
        }

        compile_stmt_block(s->block, ctx);

        if (!ctx->bl->GetInsertBlock()->getTerminator()) {
                llvm::Type *return_type = existing_function->getReturnType();
                if (return_type->isVoidTy()) {
                        ctx->bl->CreateRetVoid();
                } else if (return_type->isIntegerTy(32)) {
                        ctx->bl->CreateRet(llvm::ConstantInt::get(return_type, 0));
                } else {
                        printf("function %s with non-void return type lacks a return statement", s->id->lx);
                        ctx->bl->CreateUnreachable();
                }
        }

        pop_scope(ctx);

        llvm::verifyFunction(*existing_function);
        return existing_function;
}

static llvm::Value *compile_stmt_let(Stmt_Let *s, Context *ctx) {
        llvm::Type *llty = scr_type_to_llvm_type(s->type, ctx); // i32 for z
        llvm::AllocaInst *alloca_inst = ctx->bl->CreateAlloca(llty, nullptr, s->id->lx); // ptr to i32

        llvm::Value *init_value = compile_expr(s->e, ctx);
        if (!init_value) {
                err_wargs("failed to compile expression for identifier %s", s->id->lx);
        }

        // If init_value is a pointer to a variable (from an identifier), load its value
        if (init_value->getType()->isPointerTy() && s->e->ty == EXPR_TYPE_IDENT) {
                char *id = ((Expr_Ident *)s->e)->id->lx;
                for (int i = (int)ctx->vs.length() - 1; i >= 0; --i) {
                        if (ctx->vs[i]->has(id)) {
                                Var *var = *(ctx->vs[i]->get(id));
                                init_value = ctx->bl->CreateLoad(scr_type_to_llvm_type(var->ty, ctx), init_value, id);
                                break;
                        }
                }
        }

        Var *new_var = new Var{ s->id, s->type, alloca_inst };
        add_var_to_scope(new_var, ctx);

        return ctx->bl->CreateStore(init_value, alloca_inst);
}

static llvm::Value *compile_stmt_return(Stmt_Return *s, Context *ctx) {
    if (s->e) {
        llvm::Value *v = compile_expr(s->e, ctx);
        if (!v) {
            err("compile_stmt_return: failed to compile return expression");
        }
        return ctx->bl->CreateRet(v);
    } else {
        return ctx->bl->CreateRetVoid();  // No expression, return void
    }
}

static llvm::Function *compile_stmt_def(Stmt_Def *s, Context *ctx) {
        return gen_proc_proto(s->proto, ctx);
}

static llvm::Value *compile_stmt_if(Stmt_If *s, Context *ctx) {
        llvm::Value *cond = compile_expr(s->e, ctx);
        if (!cond) {
                err("compile_stmt_if: could not compile condition");
        }

        if (!cond->getType()->isIntegerTy(1)) {
                cond = ctx->bl->CreateICmpNE(
                        cond,
                        llvm::ConstantInt::get(cond->getType(), 0),
                        "tobool");
        }

        llvm::Function *parent_func = ctx->bl->GetInsertBlock()->getParent();
        llvm::BasicBlock *then_bb = llvm::BasicBlock::Create(*ctx->llctx, "then", parent_func);
        llvm::BasicBlock *else_bb = s->else_ ? llvm::BasicBlock::Create(*ctx->llctx, "else") : nullptr;
        llvm::BasicBlock *merge_bb = llvm::BasicBlock::Create(*ctx->llctx, "ifcont");

        ctx->bl->CreateCondBr(cond, then_bb, else_bb);

        ctx->bl->SetInsertPoint(then_bb);

        llvm::Value *then_value = compile_stmt(s->then, ctx);

        ctx->bl->CreateBr(merge_bb);
        then_bb = ctx->bl->GetInsertBlock();

        // Emit else block
        parent_func->insert(parent_func->end(), else_bb);
        ctx->bl->SetInsertPoint(else_bb);

        llvm::Value *else_value = compile_stmt(s->else_, ctx);

        ctx->bl->CreateBr(merge_bb);
        else_bb = ctx->bl->GetInsertBlock();

        // Emit merge block
        parent_func->insert(parent_func->end(), merge_bb);
        ctx->bl->SetInsertPoint(merge_bb);
        llvm::PHINode *pn = ctx->bl->CreatePHI(llvm::Type::getInt32Ty(*ctx->llctx), 2, "iftmp");

        pn->addIncoming(then_value, then_bb);
        pn->addIncoming(else_value, else_bb);

        return pn;
}

static llvm::Value *compile_stmt_while(Stmt_While *s, Context *ctx) {
        llvm::Function *parent_func = ctx->bl->GetInsertBlock()->getParent();

        // Create basic blocks for the while loop
        llvm::BasicBlock *loop_cond_bb = llvm::BasicBlock::Create(*ctx->llctx, "while_cond", parent_func);
        llvm::BasicBlock *loop_body_bb = llvm::BasicBlock::Create(*ctx->llctx, "while_body", parent_func);
        llvm::BasicBlock *loop_end_bb = llvm::BasicBlock::Create(*ctx->llctx, "while_end");

        // Branch from current block to condition check
        ctx->bl->CreateBr(loop_cond_bb);

        // Start inserting into condition block
        ctx->bl->SetInsertPoint(loop_cond_bb);

        // Compile the condition expression
        llvm::Value *cond = compile_expr(s->e, ctx);
        if (!cond) {
                err("compile_stmt_while: could not compile condition");
        }

        // Ensure condition is a boolean (i1) value
        if (!cond->getType()->isIntegerTy(1)) {
                cond = ctx->bl->CreateICmpNE(cond,
                                             llvm::ConstantInt::get(cond->getType(), 0),
                                             "whilecond_tobool");
        }

        // Conditional branch based on the condition
        ctx->bl->CreateCondBr(cond, loop_body_bb, loop_end_bb);

        ctx->bl->SetInsertPoint(loop_body_bb);
        llvm::Value *body_value = compile_stmt(s->s, ctx);

        // Branch back to condition check (loop back)
        ctx->bl->CreateBr(loop_cond_bb);

        // Set insertion point to exit block
        parent_func->insert(parent_func->end(), loop_end_bb);
        ctx->bl->SetInsertPoint(loop_end_bb);

        // Undefined value of i32 type to maintain consistency
        return llvm::UndefValue::get(llvm::Type::getInt32Ty(*ctx->llctx));
}

static llvm::Value *compile_stmt_for(Stmt_For *s, Context *ctx) {
        llvm::Function *parent_func = ctx->bl->GetInsertBlock()->getParent();

        // Create basic blocks for the for loop structure
        llvm::BasicBlock *init_bb = llvm::BasicBlock::Create(*ctx->llctx, "for_init", parent_func);
        llvm::BasicBlock *cond_bb = llvm::BasicBlock::Create(*ctx->llctx, "for_cond");
        llvm::BasicBlock *body_bb = llvm::BasicBlock::Create(*ctx->llctx, "for_body");
        llvm::BasicBlock *update_bb = llvm::BasicBlock::Create(*ctx->llctx, "for_update");
        llvm::BasicBlock *end_bb = llvm::BasicBlock::Create(*ctx->llctx, "for_end");

        // Branch from current block to initialization
        ctx->bl->CreateBr(init_bb);

        // Initialization block
        ctx->bl->SetInsertPoint(init_bb);
        if (s->init) {
                compile_stmt(s->init, ctx);
        }
        ctx->bl->CreateBr(cond_bb);

        // Condition block
        parent_func->insert(parent_func->end(), cond_bb);
        ctx->bl->SetInsertPoint(cond_bb);
        llvm::Value *cond = compile_expr(s->cond, ctx);
        if (!cond) {
                err("compile_stmt_for: could not compile condition");
        }

        // Ensure condition is a boolean (i1) value
        if (!cond->getType()->isIntegerTy(1)) {
                cond = ctx->bl->CreateICmpNE(
                        cond,
                        llvm::ConstantInt::get(cond->getType(), 0),
                        "forcond_tobool");
        }
        ctx->bl->CreateCondBr(cond, body_bb, end_bb);

        // Body block
        parent_func->insert(parent_func->end(), body_bb);
        ctx->bl->SetInsertPoint(body_bb);
        compile_stmt(s->body, ctx);
        ctx->bl->CreateBr(update_bb);

        // Update block
        parent_func->insert(parent_func->end(), update_bb);
        ctx->bl->SetInsertPoint(update_bb);
        compile_expr(s->end, ctx);
        ctx->bl->CreateBr(cond_bb);  // Loop back to condition

        // End block
        parent_func->insert(parent_func->end(), end_bb);
        ctx->bl->SetInsertPoint(end_bb);

        // Return an undefined value to maintain consistency with other statements
        return llvm::UndefValue::get(llvm::Type::getInt32Ty(*ctx->llctx));
}

static llvm::Value *compile_stmt_struct(Stmt_Struct *s, Context *ctx) {
        // Collect the LLVM types for all fields in the struct
        std::vector<llvm::Type *> field_types;
        for (size_t i = 0; i < s->fields.len; ++i) {
                llvm::Type *field_type = scr_type_to_llvm_type(s->fields.types[i], ctx);
                if (!field_type) {
                        err_wargs("failed to convert type for field %s in struct %s",
                                  s->fields.ids[i]->lx, s->id->lx);
                        return nullptr;
                }
                field_types.push_back(field_type);
        }

        // Create or get the LLVM struct type by name
        llvm::StructType *struct_type = llvm::StructType::getTypeByName(*(ctx->llctx), s->id->lx);
        if (!struct_type) {
                struct_type = llvm::StructType::create(*(ctx->llctx), field_types, s->id->lx);
        } else {
                // Verify the existing type matches the definition (optional for stricter checking)
                if (struct_type->getNumElements() != field_types.size()) {
                        err_wargs("redefinition of struct %s with different fields", s->id->lx);
                        return nullptr;
                }
                // Could add more type checking here if needed
        }

        // Register the struct in the context's struct table
        for (size_t i = 0; i < ctx->ss.length(); ++i) {
                if (ctx->ss[i].has(s->id->lx)) {
                        err_wargs("struct %s is already defined", s->id->lx);
                        return nullptr;
                }
        }

        // Add the struct to the current scope's struct map
        Umap<char *, Stmt_Struct *> *struct_map;
        if (ctx->ss.length() == 0 || ctx->ss.back().empty()) {
                struct_map = new Umap<char *, Stmt_Struct *>([](char *s0, char *s1) {
                        return !strcmp(s0, s1);
                });
                ctx->ss.add(*struct_map);
        } else {
                struct_map = &ctx->ss.back();
        }
        struct_map->add(s->id->lx, s);

        // No runtime value for struct definitions
        return nullptr;
}

static llvm::Value *compile_stmt(Stmt *s, Context *ctx) {
        switch (s->ty) {
        case STMT_TYPE_LET: {
                return compile_stmt_let((Stmt_Let *)s, ctx);
        } break;
        case STMT_TYPE_PROC: {
                (void)compile_stmt_proc((Stmt_Proc *)s, ctx);
                return nullptr;
        } break;
        case STMT_TYPE_BLOCK: {
                return compile_stmt_block((Stmt_Block *)s, ctx);
        } break;
        case STMT_TYPE_RETURN: {
                return compile_stmt_return((Stmt_Return *)s, ctx);
        } break;
        case STMT_TYPE_DEF: {
                (void)compile_stmt_def((Stmt_Def *)s, ctx);
                return nullptr;
        } break;
        case STMT_TYPE_EXPR: {
                return compile_expr(((Stmt_Expr *)s)->e, ctx);
        } break;
        case STMT_TYPE_IF: {
                return compile_stmt_if((Stmt_If *)s, ctx); // No need to store the return value unless needed
        } break;
        case STMT_TYPE_EMPTY: {
                return llvm::UndefValue::get(llvm::Type::getInt32Ty(*ctx->llctx));
        } break;
        case STMT_TYPE_WHILE: {
                return compile_stmt_while((Stmt_While *)s, ctx);
        } break;
        case STMT_TYPE_FOR: {
                return compile_stmt_for((Stmt_For *)s, ctx);
        } break;
        case STMT_TYPE_STRUCT: {
                return compile_stmt_struct((Stmt_Struct *)s, ctx);
        } break;
        default: {
                err_wargs("unknown statement: %d", (int)s->ty);
        } break;
        }
}

void codegen(Program *p) {
        assert(0 && "unimplemented");

        Context *ctx = new Context;
        ctx->llctx = new llvm::LLVMContext();
        ctx->md = new llvm::Module("main", *ctx->llctx);
        ctx->bl = new llvm::IRBuilder<>(*ctx->llctx);

        for (size_t i = 0; i < p->len; ++i) {
                (void)compile_stmt(p->stmts[i], ctx);
        }

        if (llvm::verifyModule(*(ctx->md), &llvm::errs())) {
                llvm::errs() << "Error: Module verification failed\n";
                llvm::errs() << "Module contents";
                ctx->md->print(llvm::errs(), nullptr);
                exit(1);
        }

        LLVMInitializeX86TargetInfo();
        LLVMInitializeX86Target();
        LLVMInitializeX86TargetMC();
        LLVMInitializeX86AsmParser();
        LLVMInitializeX86AsmPrinter();

        std::string target_triple = llvm::sys::getDefaultTargetTriple();
        ctx->md->setTargetTriple(target_triple);

        std::string error;
        const llvm::Target *target = llvm::TargetRegistry::lookupTarget(target_triple, error);
        if (!target) {
                llvm::errs() << "Error: " << error << "\n";
                exit(1);
        }

        llvm::TargetOptions options;
        auto cpu = "generic";
        auto features = "";
        llvm::TargetMachine *target_machine = target->createTargetMachine(target_triple, cpu,
                                                                          features, options, llvm::Reloc::PIC_);
        ctx->md->setDataLayout(target_machine->createDataLayout());

        std::error_code ec;
        llvm::raw_fd_ostream dest("scr_output.o", ec, llvm::sys::fs::OF_None);
        if (ec) {
                llvm::errs() << "Could not open file: " << ec.message() << "\n";
                exit(1);
        }

        llvm::legacy::PassManager pass;
        if (target_machine->addPassesToEmitFile(pass, dest, nullptr, llvm::CGFT_ObjectFile)) {
                llvm::errs() << "TargetMachine can't emit an object file\n";
                exit(1);
        }

        pass.run(*(ctx->md));
        dest.flush();
        dest.close();

        std::string link_command = "gcc -o scr_output scr_output.o";
        int link_result = system(link_command.c_str());
        if (link_result != 0) {
                llvm::errs() << "Error: Linking failed\n";
                exit(1);
        }

        // delete ctx->bl;
        // delete ctx->md;
        // delete ctx->llctx;
        // delete ctx;
}

