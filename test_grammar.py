import sys
import re

# Token classes corresponding to the scanner
class Token:
    def __init__(self, pos, cls, val=None):
        self.pos = pos
        self.cls = cls
        self.val = val

    def __repr__(self):
        if self.val is not None:
            return f"Token {self.pos} ({self.cls} {self.val!r})"
        return f"Token {self.pos} {self.cls}"

def scan(source):
    tokens = []
    lines = source.splitlines(keepends=True)
    line_num = 1
    for line in lines:
        col = 1
        i = 0
        while i < len(line):
            c = line[i]
            # Whitespace
            if c in ' \t\r\n':
                i += 1
                col += 1
                continue
            # Comment
            if c == '#':
                break
            pos = f"(AlexPn 0 {line_num} {col})"
            # Two-char operators
            two = line[i:i+2]
            if two == '<=':
                tokens.append(Token(pos, 'LessEqual'))
                i += 2; col += 2; continue
            elif two == '>=':
                tokens.append(Token(pos, 'GreaterEqual'))
                i += 2; col += 2; continue
            elif two == '==':
                tokens.append(Token(pos, 'Equal'))
                i += 2; col += 2; continue
            elif two == '!=':
                tokens.append(Token(pos, 'NotEqual'))
                i += 2; col += 2; continue
            elif two == '+=':
                tokens.append(Token(pos, 'PlusAssign'))
                i += 2; col += 2; continue
            elif two == '-=':
                tokens.append(Token(pos, 'MinusAssign'))
                i += 2; col += 2; continue
            elif two == '*=':
                tokens.append(Token(pos, 'MultAssign'))
                i += 2; col += 2; continue
            elif two == '/=':
                tokens.append(Token(pos, 'DivAssign'))
                i += 2; col += 2; continue
            elif two == '**':
                tokens.append(Token(pos, 'Power'))
                i += 2; col += 2; continue

            # Single char operators & punctuation
            if c == '{': tokens.append(Token(pos, 'OpenBrace')); i += 1; col += 1; continue
            if c == '}': tokens.append(Token(pos, 'CloseBrace')); i += 1; col += 1; continue
            if c == '(': tokens.append(Token(pos, 'OpenParen')); i += 1; col += 1; continue
            if c == ')': tokens.append(Token(pos, 'CloseParen')); i += 1; col += 1; continue
            if c == '[': tokens.append(Token(pos, 'OpenBracket')); i += 1; col += 1; continue
            if c == ']': tokens.append(Token(pos, 'CloseBracket')); i += 1; col += 1; continue
            if c == ',': tokens.append(Token(pos, 'Comma')); i += 1; col += 1; continue
            if c == ';': tokens.append(Token(pos, 'SemiColon')); i += 1; col += 1; continue
            if c == ':': tokens.append(Token(pos, 'Colon')); i += 1; col += 1; continue
            if c == '=': tokens.append(Token(pos, 'Assign')); i += 1; col += 1; continue
            if c == '+': tokens.append(Token(pos, 'Plus')); i += 1; col += 1; continue
            if c == '-': tokens.append(Token(pos, 'Minus')); i += 1; col += 1; continue
            if c == '*': tokens.append(Token(pos, 'Multiply')); i += 1; col += 1; continue
            if c == '/': tokens.append(Token(pos, 'Divide')); i += 1; col += 1; continue
            if c == '%': tokens.append(Token(pos, 'Modulo')); i += 1; col += 1; continue
            if c == '@': tokens.append(Token(pos, 'MatrixMult')); i += 1; col += 1; continue
            if c == '^': tokens.append(Token(pos, 'Caret')); i += 1; col += 1; continue
            if c == '<': tokens.append(Token(pos, 'Less')); i += 1; col += 1; continue
            if c == '>': tokens.append(Token(pos, 'Greater')); i += 1; col += 1; continue

            # String literal
            if c == '"':
                m = re.match(r'"([^"\\\n]*)"', line[i:])
                if m:
                    content = m.group(1)
                    tokens.append(Token(pos, 'StringLiteral', content))
                    i += len(m.group(0))
                    col += len(m.group(0))
                    continue
                else:
                    raise SyntaxError(f"Unclosed string literal at {line_num}:{col}")

            # Digits
            if c.isdigit():
                m = re.match(r'(\d+\.\d+|\d+)', line[i:])
                val_str = m.group(1)
                if '.' in val_str:
                    tokens.append(Token(pos, 'FloatLiteral', float(val_str)))
                else:
                    tokens.append(Token(pos, 'IntLiteral', int(val_str)))
                i += len(val_str)
                col += len(val_str)
                continue

            # Identifier or keyword
            if c.isalpha() or c == '_':
                m = re.match(r'[a-zA-Z_][a-zA-Z0-9_]*', line[i:])
                ident = m.group(0)
                kw_map = {
                    'def': 'Def',
                    'model': 'Model',
                    'enum': 'Enum',
                    'const': 'Const',
                    'if': 'If',
                    'else': 'Else',
                    'for': 'For',
                    'while': 'While',
                    'return': 'Return',
                    'print': 'Print',
                    'and': 'And',
                    'or': 'Or',
                    'not': 'Not',
                    'in': 'In',
                    'cat': 'Cat',
                    'true': ('BoolLiteral', True),
                    'false': ('BoolLiteral', False)
                }
                if ident in kw_map:
                    mapped = kw_map[ident]
                    if isinstance(mapped, tuple):
                        tokens.append(Token(pos, mapped[0], mapped[1]))
                    else:
                        tokens.append(Token(pos, mapped))
                else:
                    tokens.append(Token(pos, 'Identifier', ident))
                i += len(ident)
                col += len(ident)
                continue

            # Lexical error
            tokens.append(Token(pos, 'LexicalError', c))
            i += 1
            col += 1
        line_num += 1
    return tokens

import os

def find_file(fn):
    if os.path.exists(fn):
        return fn
    alt = os.path.join("corbieProgrammingLanguage", fn)
    if os.path.exists(alt):
        return alt
    raise FileNotFoundError(f"File {fn} not found")

# Test scan of quickSortV1.pe and quickSortV2.pe
with open(find_file("quickSortV1.pe")) as f:
    v1_toks = scan(f.read())
print(f"quickSortV1 tokens scanned: {len(v1_toks)}")

with open(find_file("quickSortV2.pe")) as f:
    v2_toks = scan(f.read())
print(f"quickSortV2 tokens scanned: {len(v2_toks)}")

class SemanticError(Exception):
    pass

BUILTINS = {
    "input":     (0, 1, "TDynamic"),
    "len":       (1, 1, "TInt"),
    "split":     (2, 2, ("TList", "ShapeUnknown")),
    "zeros":     (1, None, ("TList", "ShapeUnknown")),
    "ones":      (1, None, ("TList", "ShapeUnknown")),
    "reshape":   (2, 2, ("TList", "ShapeUnknown")),
    "dot":       (2, 2, "TDynamic"),
    "to_int":    (1, 1, "TInt"),
    "to_float":  (1, 1, "TFloat"),
    "to_string": (1, 1, "TString"),
    "to_bool":   (1, 1, "TBool"),
    "int":       (1, 1, "TInt"),
    "float":     (1, 1, "TFloat"),
    "str":       (1, 1, "TString"),
    "bool":      (1, 1, "TBool"),
}

# Parser + Semantic Analyzer mimicking Parsec combinators and ParserState
class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.pos = 0
        self.symtable = []          # [(Token(..., 'escopo#nome'), Token)]
        self.scope_stack = ["global"]
        self.scope_counter = 0
        self.var_types = {}
        self.const_set = set()
        self.const_zeros = set()
        self.func_sigs = dict(BUILTINS)
        self.in_function = False

    def current(self):
        if self.pos < len(self.tokens):
            return self.tokens[self.pos]
        return None

    def match(self, cls, val=None):
        tok = self.current()
        if tok and tok.cls == cls:
            if val is None or tok.val == val:
                self.pos += 1
                return tok
        return None

    def expect(self, cls, val=None):
        tok = self.match(cls, val)
        if not tok:
            cur = self.current()
            raise SyntaxError(f"Expected {cls} {val or ''}, got {cur} at token {self.pos}")
        return tok

    def current_scope(self):
        return self.scope_stack[0] if self.scope_stack else "global"

    def enter_named_scope(self, name):
        self.scope_stack.insert(0, name)

    def enter_anon_scope(self, prefix):
        self.scope_counter += 1
        self.scope_stack.insert(0, f"{prefix}_{self.scope_counter}")

    def exit_scope(self):
        if len(self.scope_stack) > 1:
            self.scope_stack.pop(0)

    def symtable_lookup_scoped(self, name):
        for scope in self.scope_stack:
            key = f"{scope}#{name}"
            for t1, t2 in self.symtable:
                if t1.val == key:
                    return (key, (t1, t2))
        return None

    def symtable_insert_scoped(self, name_tok, val_tok, typ="TDynamic", is_const=False):
        scope = self.current_scope()
        key = f"{scope}#{name_tok.val}"
        for t1, _ in self.symtable:
            if t1.val == key:
                raise SemanticError(f"variable already declared: {key}")
        scoped_tok = Token(name_tok.pos, 'Identifier', key)
        self.symtable.insert(0, (scoped_tok, val_tok))
        self.var_types[key] = typ
        if is_const:
            self.const_set.add(key)
        return key

    def symtable_update_scoped(self, key, name_tok, val_tok, typ):
        scoped_tok = Token(name_tok.pos, 'Identifier', key)
        for idx, (t1, _) in enumerate(self.symtable):
            if t1.val == key:
                self.symtable[idx] = (scoped_tok, val_tok)
                self.var_types[key] = typ
                return
        raise SemanticError(f"variable not found: {key}")

    def parse_program(self):
        stmts = []
        while self.current() is not None:
            stmts.append(self.parse_stmt())
        return stmts

    def parse_block_stmts(self):
        self.expect('OpenBrace')
        stmts = []
        while self.current() and self.current().cls != 'CloseBrace':
            stmts.append(self.parse_stmt())
        self.expect('CloseBrace')
        return stmts

    def parse_scoped_block(self, prefix):
        self.expect('OpenBrace')
        self.enter_anon_scope(prefix)
        stmts = []
        while self.current() and self.current().cls != 'CloseBrace':
            stmts.append(self.parse_stmt())
        self.expect('CloseBrace')
        self.exit_scope()
        return stmts

    def parse_stmt(self):
        tok = self.current()
        if not tok:
            raise SyntaxError("Unexpected EOF")

        if tok.cls == 'Def':
            return self.parse_func_def()
        elif tok.cls == 'If':
            return self.parse_if()
        elif tok.cls == 'While':
            return self.parse_while()
        elif tok.cls == 'For':
            return self.parse_for()
        elif tok.cls == 'Return':
            return self.parse_return()
        elif tok.cls == 'Print' or (tok.cls == 'Identifier' and tok.val == 'print'):
            return self.parse_print()
        elif tok.cls == 'Const':
            return self.parse_const()
        elif tok.cls == 'Model':
            return self.parse_model()
        elif tok.cls == 'Enum':
            return self.parse_enum()
        else:
            next_tok = self.tokens[self.pos + 1] if self.pos + 1 < len(self.tokens) else None
            if tok.cls == 'Identifier' and next_tok and next_tok.cls == 'OpenParen':
                fc = self.parse_func_call()
                self.expect('SemiColon')
                self.check_func_call(fc[1], fc[2])
                return ('FuncCallStmt', fc)
            assign = self.parse_assign()
            self.expect('SemiColon')
            return ('AssignStmt', assign)

    def parse_const(self):
        self.expect('Const')
        name_tok = self.expect('Identifier')
        self.expect('Assign')
        e, val_tok = self.parse_const_val()
        self.expect('SemiColon')
        typ, is_zero = self.check_const_expr(e)
        key = self.symtable_insert_scoped(name_tok, val_tok, typ=typ, is_const=True)
        if is_zero:
            self.const_zeros.add(key)
        return ('ConstDecl', name_tok.val, e)

    def check_const_expr(self, e):
        kind = e[0]
        if kind == 'LitExpr':
            cls, val = e[1]
            typ = {'IntLiteral': 'TInt', 'FloatLiteral': 'TFloat', 'StringLiteral': 'TString', 'BoolLiteral': 'TBool'}[cls]
            is_zero = (cls in ('IntLiteral', 'FloatLiteral') and val == 0)
            return (typ, is_zero)
        if kind == 'VarExpr' and e[1][0] == 'SimpleVar':
            ref = e[1][1]
            found = self.symtable_lookup_scoped(ref)
            if not found:
                raise SemanticError(f"undeclared constant '{ref}'")
            key, _ = found
            if key not in self.const_set:
                raise SemanticError(f"constant initializer '{ref}' is not a constant")
            return (self.var_types.get(key, 'TDynamic'), key in self.const_zeros)
        raise SemanticError("constant must be initialized with a literal or another constant")

    def parse_const_val(self):
        tok = self.current()
        if tok and tok.cls in ('IntLiteral', 'FloatLiteral', 'StringLiteral', 'BoolLiteral'):
            self.pos += 1
            return (('LitExpr', (tok.cls, tok.val)), tok)
        elif tok and tok.cls == 'Identifier':
            self.pos += 1
            return (('VarExpr', ('SimpleVar', tok.val)), tok)
        raise SyntaxError(f"Expected literal or identifier in const declaration, got {tok}")

    def parse_model(self):
        model_tok = self.expect('Model')
        name_tok = self.expect('Identifier')
        self.symtable_insert_scoped(name_tok, model_tok, typ=f"TModel:{name_tok.val}")
        self.enter_named_scope(name_tok.val)
        body = self.parse_block_stmts()
        self.exit_scope()
        return ('ModelDef', name_tok.val, body)

    def parse_enum(self):
        enum_tok = self.expect('Enum')
        name_tok = self.expect('Identifier')
        self.expect('OpenBrace')
        members = [self.expect('Identifier')]
        while self.match('Comma'):
            members.append(self.expect('Identifier'))
        self.expect('CloseBrace')
        enum_t = f"TEnum:{name_tok.val}"
        self.symtable_insert_scoped(name_tok, enum_tok, typ=enum_t, is_const=True)
        for m in members:
            self.symtable_insert_scoped(m, enum_tok, typ=enum_t, is_const=True)
        return ('EnumDef', name_tok.val, [m.val for m in members])

    def parse_func_def(self):
        def_tok = self.expect('Def')
        name_tok = self.expect('Identifier')
        self.expect('OpenParen')
        param_toks = []
        if self.current() and self.current().cls == 'Identifier':
            param_toks.append(self.expect('Identifier'))
            while self.match('Comma'):
                param_toks.append(self.expect('Identifier'))
        self.expect('CloseParen')

        scoped_fn = f"{self.current_scope()}#{name_tok.val}"
        self.symtable_insert_scoped(name_tok, def_tok, typ="TDynamic")
        arity = len(param_toks)
        self.func_sigs[scoped_fn] = (arity, arity, "TDynamic")

        prev_in_func = self.in_function
        self.in_function = True
        self.enter_named_scope(name_tok.val)
        for pt in param_toks:
            self.symtable_insert_scoped(pt, def_tok, typ="TDynamic")
        body = self.parse_block_stmts()
        self.exit_scope()
        self.in_function = prev_in_func
        return ('FuncDef', name_tok.val, [pt.val for pt in param_toks], body)

    def parse_if(self):
        self.expect('If')
        self.expect('OpenParen')
        cond = self.parse_logic_expr()
        self.expect('CloseParen')
        self.check_bool_condition('if', self.eval_expr(cond))
        then_b = self.parse_scoped_block('if')
        else_b = None
        if self.match('Else'):
            if self.current() and self.current().cls == 'If':
                else_b = [self.parse_if()]
            else:
                else_b = self.parse_scoped_block('else')
        return ('IfStmt', cond, then_b, else_b)

    def parse_while(self):
        self.expect('While')
        self.expect('OpenParen')
        cond = self.parse_logic_expr()
        self.expect('CloseParen')
        self.check_bool_condition('while', self.eval_expr(cond))
        body = self.parse_scoped_block('while')
        return ('WhileStmt', cond, body)

    def parse_for(self):
        self.expect('For')
        self.expect('OpenParen')
        t0 = self.current()
        t1 = self.tokens[self.pos + 1] if self.pos + 1 < len(self.tokens) else None
        if t0 and t0.cls == 'Identifier' and t1 and t1.cls == 'In':
            var_tok = self.expect('Identifier')
            in_tok = self.expect('In')
            src = self.parse_expr()
            self.expect('CloseParen')
            src_t = self.eval_expr(src)
            if not self.is_iterable(src_t):
                raise SemanticError(f"'for ... in' source must be iterable, got {src_t}")
            self.enter_anon_scope('for')
            self.symtable_insert_scoped(var_tok, in_tok, typ="TDynamic")
            body = self.parse_block_stmts()
            self.exit_scope()
            return ('ForInStmt', var_tok.val, src, body)
        else:
            self.enter_anon_scope('for')
            init = self.parse_assign()
            self.expect('SemiColon')
            cond = self.parse_logic_expr()
            self.check_bool_condition('for', self.eval_expr(cond))
            self.expect('SemiColon')
            step = self.parse_assign()
            self.expect('CloseParen')
            body = self.parse_block_stmts()
            self.exit_scope()
            return ('ForStmt', init, cond, step, body)

    def parse_return(self):
        self.expect('Return')
        val = None
        if self.current() and self.current().cls != 'SemiColon':
            val = self.parse_logic_expr()
        self.expect('SemiColon')
        if not self.in_function:
            raise SemanticError("'return' statement not allowed outside of a function")
        if val is not None:
            self.eval_expr(val)
        return ('ReturnStmt', val)

    def parse_print(self):
        if self.match('Print') is None:
            id_t = self.expect('Identifier')
            assert id_t.val == 'print'
        self.expect('OpenParen')
        e = self.parse_logic_expr()
        self.expect('CloseParen')
        self.expect('SemiColon')
        self.eval_expr(e)
        return ('PrintStmt', e)

    def parse_assign(self):
        v, base_tok = self.parse_var()
        op = None
        for cand in ['Assign', 'PlusAssign', 'MinusAssign', 'MultAssign', 'DivAssign']:
            if self.match(cand):
                op = cand
                break
        if not op:
            raise SyntaxError(f"Expected assign op, got {self.current()}")
        e = self.parse_logic_expr()
        self.check_assign(base_tok, v, op, e)
        return ('Assign', v, op, e)

    def check_assign(self, base_tok, v, op, rhs):
        rhs_type = self.eval_expr(rhs)
        val_tok = Token(base_tok.pos, op)
        if v[0] == 'IndexVar':
            key, _ = self.check_var_chain(v)
            base_t = self.var_types.get(key, 'TDynamic')
            if key in self.const_set:
                raise SemanticError(f"cannot mutate constant '{base_tok.val}'")
            if base_t == 'TString':
                raise SemanticError(f"strings are immutable and cannot be assigned by index ('{base_tok.val}')")
            if op != 'Assign':
                self.check_compound_compat(op, 'TDynamic', rhs_type)
            return
        found = self.symtable_lookup_scoped(base_tok.val)
        if found:
            key, _ = found
            if key in self.const_set:
                raise SemanticError(f"cannot assign to constant '{base_tok.val}'")
            cur_type = self.var_types.get(key, 'TDynamic')
            if op == 'Assign':
                new_type = rhs_type
            else:
                self.check_compound_compat(op, cur_type, rhs_type)
                new_type = self.combine_compound_type(op, cur_type, rhs_type)
            self.symtable_update_scoped(key, base_tok, val_tok, new_type)
        else:
            if op != 'Assign':
                raise SemanticError(f"undeclared variable '{base_tok.val}' in compound assignment")
            self.symtable_insert_scoped(base_tok, val_tok, typ=rhs_type)

    def check_compound_compat(self, op, lt, rt):
        bin_map = {'PlusAssign': 'OpAdd', 'MinusAssign': 'OpSub', 'MultAssign': 'OpMul', 'DivAssign': 'OpDiv'}
        self.eval_binary_type(bin_map[op], lt, rt)

    def combine_compound_type(self, op, lt, rt):
        if op == 'PlusAssign':
            if lt == 'TInt' and rt == 'TInt': return 'TInt'
            if (lt == 'TFloat' or rt == 'TFloat') and self.is_numeric(lt) and self.is_numeric(rt): return 'TFloat'
            if lt == 'TString' and rt == 'TString': return 'TString'
            if self.is_list(lt) and self.is_list(rt): return ('TList', 'ShapeUnknown')
        if op == 'DivAssign' and lt == 'TInt' and rt == 'TInt': return 'TInt'
        if lt == 'TFloat' or rt == 'TFloat': return 'TFloat'
        if lt == 'TInt' and rt == 'TInt': return 'TInt'
        return 'TDynamic'

    def lookup_func_sig(self, fn_name):
        for scope in self.scope_stack:
            key = f"{scope}#{fn_name}"
            if key in self.func_sigs:
                return self.func_sigs[key]
        return self.func_sigs.get(fn_name)

    def check_func_call(self, fn_name, args):
        sig = self.lookup_func_sig(fn_name)
        if not sig:
            raise SemanticError(f"undeclared function '{fn_name}'")
        min_a, max_a, ret_t = sig
        if len(args) < min_a or (max_a is not None and len(args) > max_a):
            raise SemanticError(f"function '{fn_name}' arity mismatch: expected ({min_a}, {max_a}), got {len(args)}")
        arg_types = [self.eval_expr(a) for a in args]
        return self.check_builtin_args(fn_name, args, arg_types, ret_t)

    def extract_pos_lit_int(self, expr_node):
        if expr_node[0] == 'IntLit':
            if expr_node[1] <= 0:
                raise SemanticError(f"matrix dimension must be positive, got {expr_node[1]}")
            return expr_node[1]
        if expr_node[0] == 'UnaryOp' and expr_node[1] == 'OpNeg' and expr_node[2][0] == 'IntLit':
            val = -expr_node[2][1]
            if val <= 0:
                raise SemanticError(f"matrix dimension must be positive, got {val}")
            return val
        return None

    def check_builtin_args(self, fn, args, arg_types, ret_t):
        if fn == "len":
            if not self.is_iterable(arg_types[0]):
                raise SemanticError(f"'len' requires iterable argument, got {arg_types[0]}")
            return ret_t
        if fn == "split":
            s_t, sep_t = arg_types
            if s_t not in ('TString', 'TDynamic') or sep_t not in ('TString', 'TDynamic'):
                raise SemanticError(f"'split' requires (string, string), got ({s_t}, {sep_t})")
            return ret_t
        if fn == "input" and len(arg_types) == 1:
            if arg_types[0] not in ('TString', 'TDynamic'):
                raise SemanticError(f"'input' prompt must be string, got {arg_types[0]}")
            return ret_t
        if fn in ("zeros", "ones"):
            for t in arg_types:
                if t not in ('TInt', 'TDynamic'):
                    raise SemanticError(f"matrix dimension must be int, got {t}")
            dims = [self.extract_pos_lit_int(a) for a in args]
            if all(d is not None for d in dims):
                return ('TList', ('ShapeKnown', dims))
            return ('TList', 'ShapeUnknown')
        if fn == "reshape":
            m_t, sh_t = arg_types
            if m_t == ('TList', 'ShapeIrregular'):
                raise SemanticError("cannot reshape irregular matrix")
            if not (self.is_list(m_t) or m_t == 'TDynamic'):
                raise SemanticError(f"'reshape' first argument must be list/matrix, got {m_t}")
            if sh_t == ('TList', 'ShapeIrregular') or not (self.is_list(sh_t) or sh_t == 'TDynamic'):
                raise SemanticError(f"'reshape' shape argument must be list, got {sh_t}")
            shape_expr = args[1]
            if shape_expr[0] == 'ListLit':
                for el in shape_expr[1]:
                    et = self.eval_expr(el)
                    if et not in ('TInt', 'TDynamic'):
                        raise SemanticError(f"matrix dimension must be int, got {et}")
                new_dims = [self.extract_pos_lit_int(el) for el in shape_expr[1]]
                if all(d is not None for d in new_dims):
                    if isinstance(m_t, tuple) and isinstance(m_t[1], tuple) and m_t[1][0] in ('ShapeKnown', 'ShapeNonNumeric'):
                        old_dims = m_t[1][1]
                        old_prod = 1
                        for d in old_dims: old_prod *= d
                        new_prod = 1
                        for d in new_dims: new_prod *= d
                        if old_prod != new_prod:
                            raise SemanticError(f"cannot reshape matrix of size {old_prod} into shape {new_dims}")
                        return ('TList', (m_t[1][0], new_dims))
                    return ('TList', ('ShapeKnown', new_dims))
            return ('TList', 'ShapeUnknown')
        if fn == "dot":
            self.check_numeric_matrix_expr(args[0])
            self.check_numeric_matrix_expr(args[1])
            return self.eval_matmul("dot", arg_types[0], arg_types[1])
        if fn in ("to_int", "int", "to_float", "float"):
            t = arg_types[0]
            if t not in ('TInt', 'TFloat', 'TBool', 'TString', 'TDynamic'):
                raise SemanticError(f"cannot convert {t} to number in '{fn}'")
            return 'TInt' if fn in ('to_int', 'int') else 'TFloat'
        return ret_t

    def check_var_chain(self, v):
        if v[0] == 'SimpleVar':
            name = v[1]
            found = self.symtable_lookup_scoped(name)
            if not found:
                raise SemanticError(f"undeclared variable '{name}'")
            key, _ = found
            return (key, self.var_types.get(key, 'TDynamic'))
        _, inner, slices = v
        key, inner_t = self.check_var_chain(inner)
        if not self.is_indexable(inner_t):
            raise SemanticError(f"type {inner_t} is not indexable")
        if inner_t == 'TString' and len(slices) > 1:
            raise SemanticError("too many indices for string")
        if inner_t == 'TDict' and len(slices) > 1:
            raise SemanticError("too many indices for dict")
        if isinstance(inner_t, tuple) and isinstance(inner_t[1], tuple) and inner_t[1][0] in ('ShapeKnown', 'ShapeNonNumeric'):
            dims = inner_t[1][1]
            if len(slices) > len(dims):
                raise SemanticError(f"too many indices for list/matrix: expected at most {len(dims)}, got {len(slices)}")
        for s in slices:
            if s[0] == 'SliceSingle':
                idx_t = self.eval_expr(s[1])
                if inner_t == 'TDict':
                    if idx_t not in ('TString', 'TDynamic'):
                        raise SemanticError(f"dictionary key must be string, got {idx_t}")
                elif idx_t not in ('TInt', 'TDynamic'):
                    raise SemanticError(f"index must be int, got {idx_t}")
            else:
                if inner_t == 'TDict':
                    raise SemanticError("slicing with ':' not supported on dict")
                for bound in (s[1], s[2]):
                    if bound is not None:
                        bt = self.eval_expr(bound)
                        if bt not in ('TInt', 'TDynamic'):
                            raise SemanticError(f"slice bound must be int, got {bt}")
        if inner_t == 'TString':
            return (key, 'TString')
        if isinstance(inner_t, tuple) and isinstance(inner_t[1], tuple) and inner_t[1][0] in ('ShapeKnown', 'ShapeNonNumeric'):
            if any(s[0] == 'SliceFromTo' for s in slices):
                return (key, ('TList', 'ShapeUnknown'))
            rem = inner_t[1][1][len(slices):]
            return (key, 'TDynamic' if not rem else ('TList', (inner_t[1][0], rem)))
        return (key, 'TDynamic')

    def is_numeric(self, t):
        return t in ('TInt', 'TFloat', 'TDynamic')

    def is_list(self, t):
        return isinstance(t, tuple) and t[0] == 'TList'

    def is_indexable(self, t):
        return self.is_list(t) or t in ('TString', 'TDict', 'TDynamic')

    def is_iterable(self, t):
        return self.is_list(t) or t in ('TString', 'TDict', 'TDynamic')

    def check_bool_condition(self, ctx, t):
        if t not in ('TBool', 'TDynamic'):
            raise SemanticError(f"condition in '{ctx}' must be boolean, got {t}")

    def infer_list_type(self, elems):
        if not elems:
            return ('TList', ('ShapeKnown', [0]))
        elem_types = [self.eval_expr(el) for el in elems]
        if ('TList', 'ShapeIrregular') in elem_types:
            return ('TList', 'ShapeIrregular')
        if all(isinstance(t, tuple) and isinstance(t[1], tuple) and t[1][0] in ('ShapeKnown', 'ShapeNonNumeric') for t in elem_types):
            dims = [t[1][1] for t in elem_types]
            if all(d == dims[0] for d in dims[1:]):
                non_num = any(t[1][0] == 'ShapeNonNumeric' for t in elem_types)
                ctor = 'ShapeNonNumeric' if non_num else 'ShapeKnown'
                return ('TList', (ctor, [len(elems)] + dims[0]))
            return ('TList', 'ShapeIrregular')
        if any(self.is_list(t) for t in elem_types):
            if all(self.is_list(t) or t == 'TDynamic' for t in elem_types):
                return ('TList', 'ShapeUnknown')
            return ('TList', 'ShapeIrregular')
        if any(t not in ('TInt', 'TFloat', 'TDynamic') for t in elem_types):
            return ('TList', ('ShapeNonNumeric', [len(elems)]))
        if 'TDynamic' in elem_types:
            return ('TList', 'ShapeUnknown')
        return ('TList', ('ShapeKnown', [len(elems)]))

    def check_numeric_matrix_expr(self, expr_node):
        if expr_node[0] == 'ListLit':
            for el in expr_node[1]:
                if el[0] == 'ListLit':
                    self.check_numeric_matrix_expr(el)
                else:
                    t = self.eval_expr(el)
                    if not (self.is_numeric(t) or self.is_list(t)):
                        raise SemanticError(f"matrix elements must be numeric, got {t}")

    def eval_matmul(self, op_name, lt, rt):
        if lt == ('TList', 'ShapeIrregular') or rt == ('TList', 'ShapeIrregular'):
            raise SemanticError(f"operator '{op_name}' cannot be applied to irregular matrix")
        if (isinstance(lt, tuple) and isinstance(lt[1], tuple) and lt[1][0] == 'ShapeNonNumeric') or \
           (isinstance(rt, tuple) and isinstance(rt[1], tuple) and rt[1][0] == 'ShapeNonNumeric'):
            raise SemanticError(f"matrix elements must be numeric for '{op_name}'")
        if not ((self.is_list(lt) or lt == 'TDynamic') and (self.is_list(rt) or rt == 'TDynamic')):
            raise SemanticError(f"operator '{op_name}' requires matrix/list operands, got {lt} and {rt}")
        if isinstance(lt, tuple) and isinstance(lt[1], tuple) and lt[1][0] == 'ShapeKnown' and \
           isinstance(rt, tuple) and isinstance(rt[1], tuple) and rt[1][0] == 'ShapeKnown':
            s1, s2 = lt[1][1], rt[1][1]
            if not s1 or not s2 or any(d == 0 for d in s1) or any(d == 0 for d in s2):
                raise SemanticError(f"cannot multiply empty matrix in '{op_name}'")
            if len(s1) == 2 and len(s2) == 2:
                m, n = s1; p, q = s2
                if n != p:
                    raise SemanticError(f"matrix dimension mismatch in '{op_name}': cannot multiply ({m}x{n}) by ({p}x{q})")
                return ('TList', ('ShapeKnown', [m, q]))
            if len(s1) == 2 and len(s2) == 1:
                m, n = s1; p = s2[0]
                if n != p:
                    raise SemanticError(f"matrix dimension mismatch in '{op_name}': ({m}x{n}) vs vector({p})")
                return ('TList', ('ShapeKnown', [m]))
            if len(s1) == 1 and len(s2) == 2:
                n = s1[0]; p, q = s2
                if n != p:
                    raise SemanticError(f"matrix dimension mismatch in '{op_name}': vector({n}) vs ({p}x{q})")
                return ('TList', ('ShapeKnown', [q]))
            if len(s1) == 1 and len(s2) == 1:
                if s1[0] != s2[0]:
                    raise SemanticError(f"vector dimension mismatch in '{op_name}': {s1[0]} vs {s2[0]}")
                return 'TDynamic'
            inner_l = s1[-1]
            inner_r = s2[-2] if len(s2) >= 2 else s2[0]
            if inner_l != inner_r:
                raise SemanticError(f"tensor dimension mismatch in '{op_name}': {s1} vs {s2}")
            return ('TList', 'ShapeUnknown')
        return ('TList', 'ShapeUnknown')

    def is_zero_expr(self, expr_node):
        if expr_node[0] in ('IntLit', 'FloatLit') and expr_node[1] == 0:
            return True
        if expr_node[0] == 'UnaryOp' and expr_node[1] in ('OpNeg', 'OpPos'):
            return self.is_zero_expr(expr_node[2])
        if expr_node[0] == 'VarExpr' and expr_node[1][0] == 'SimpleVar':
            found = self.symtable_lookup_scoped(expr_node[1][1])
            if found and found[0] in self.const_zeros:
                return True
        return False

    def eval_expr(self, e):
        kind = e[0]
        if kind == 'IntLit': return 'TInt'
        if kind == 'FloatLit': return 'TFloat'
        if kind == 'StringLit': return 'TString'
        if kind == 'BoolLit': return 'TBool'
        if kind == 'VarExpr':
            return self.check_var_chain(e[1])[1]
        if kind == 'FuncCallExpr':
            _, fc = e
            return self.check_func_call(fc[1], fc[2])
        if kind == 'ListLit':
            return self.infer_list_type(e[1])
        if kind == 'DictLit':
            for _, val in e[1]:
                self.eval_expr(val)
            return 'TDict'
        if kind == 'ListComp':
            _, val_expr, var_name, src_expr, cond_expr = e
            src_t = self.eval_expr(src_expr)
            if not self.is_iterable(src_t):
                raise SemanticError(f"list comprehension source must be iterable, got {src_t}")
            self.enter_anon_scope('lc')
            var_tok = Token("", 'Identifier', var_name)
            in_tok = Token("", 'In')
            self.symtable_insert_scoped(var_tok, in_tok, typ='TDynamic')
            self.eval_expr(val_expr)
            if cond_expr is not None:
                self.check_bool_condition('list comprehension', self.eval_expr(cond_expr))
            self.exit_scope()
            return ('TList', 'ShapeUnknown')
        if kind == 'UnaryOp':
            _, op, sub = e
            t = self.eval_expr(sub)
            if op == 'OpNot':
                if t not in ('TBool', 'TDynamic'):
                    raise SemanticError(f"'not' requires boolean operand, got {t}")
                return 'TBool'
            if not self.is_numeric(t):
                raise SemanticError(f"unary +/- requires numeric operand, got {t}")
            return t
        if kind == 'BinaryOp':
            _, op, left, right = e
            if op == 'OpMatMul':
                self.check_numeric_matrix_expr(left)
                self.check_numeric_matrix_expr(right)
                return self.eval_matmul("@", self.eval_expr(left), self.eval_expr(right))
            if op in ('OpDiv', 'OpMod') and self.is_zero_expr(right):
                raise SemanticError("division/modulo by zero")
            lt = self.eval_expr(left)
            rt = self.eval_expr(right)
            return self.eval_binary_type(op, lt, rt)
        return 'TDynamic'

    def eval_binary_type(self, op, lt, rt):
        if op in ('OpOr', 'OpAnd'):
            if lt in ('TBool', 'TDynamic') and rt in ('TBool', 'TDynamic'):
                return 'TBool'
            raise SemanticError(f"logical '{op}' requires boolean operands, got {lt} and {rt}")
        if op == 'In':
            if rt == 'TString':
                if lt not in ('TString', 'TDynamic'):
                    raise SemanticError(f"left operand of 'in' on string must be string, got {lt}")
                return 'TBool'
            if rt == 'TDict':
                if lt not in ('TString', 'TDynamic'):
                    raise SemanticError(f"left operand of 'in' on dict must be string key, got {lt}")
                return 'TBool'
            if not self.is_iterable(rt):
                raise SemanticError(f"right operand of 'in' must be iterable, got {rt}")
            return 'TBool'
        if op in ('Equal', 'NotEqual'):
            if lt == 'TDynamic' or rt == 'TDynamic' or (self.is_numeric(lt) and self.is_numeric(rt)) or (self.is_list(lt) and self.is_list(rt)) or lt == rt:
                return 'TBool'
            raise SemanticError(f"cannot compare incompatible types {lt} and {rt}")
        if op in ('Less', 'LessEqual', 'Greater', 'GreaterEqual'):
            if lt == 'TDynamic' or rt == 'TDynamic' or (self.is_numeric(lt) and self.is_numeric(rt)) or (lt == rt == 'TString') or (self.is_list(lt) and self.is_list(rt)):
                return 'TBool'
            raise SemanticError(f"cannot order incompatible types {lt} and {rt}")
        if op == 'OpAdd':
            if lt == 'TDynamic' or rt == 'TDynamic':
                return ('TList', 'ShapeUnknown') if (self.is_list(lt) or self.is_list(rt)) else 'TDynamic'
            if lt == 'TInt' and rt == 'TInt': return 'TInt'
            if self.is_numeric(lt) and self.is_numeric(rt): return 'TFloat'
            if lt == 'TString' and rt == 'TString': return 'TString'
            if self.is_list(lt) and self.is_list(rt): return ('TList', 'ShapeUnknown')
            raise SemanticError(f"incompatible types for '+': {lt} and {rt}")
        if op == 'OpCat':
            if lt == 'TDynamic' or rt == 'TDynamic': return 'TDynamic'
            if lt == 'TString' and rt == 'TString': return 'TString'
            if self.is_list(lt) and self.is_list(rt): return ('TList', 'ShapeUnknown')
            raise SemanticError(f"incompatible types for 'cat': {lt} and {rt}")
        if op == 'OpMatMul':
            return self.eval_matmul("@", lt, rt)
        if op in ('OpSub', 'OpMul', 'OpDiv', 'OpMod', 'OpPow'):
            if self.is_numeric(lt) and self.is_numeric(rt):
                if lt == 'TDynamic' or rt == 'TDynamic': return 'TDynamic'
                return 'TFloat' if ('TFloat' in (lt, rt)) else 'TInt'
            raise SemanticError(f"arithmetic operator '{op}' requires numeric operands, got {lt} and {rt}")
        return 'TDynamic'

    def parse_var(self):
        name_tok = self.expect('Identifier')
        v = ('SimpleVar', name_tok.val)
        while self.match('OpenBracket'):
            slices = []
            slices.append(self.parse_slice())
            while self.match('Comma'):
                slices.append(self.parse_slice())
            self.expect('CloseBracket')
            v = ('IndexVar', v, slices)
        return (v, name_tok)

    def parse_slice(self):
        # <slice> → <expr> ":" <expr> | <expr> ":" | ":" <expr> | ":" | <expr>
        save = self.pos
        if self.match('Colon'):
            # ":" <expr> or ":"
            save2 = self.pos
            try:
                e = self.parse_expr()
                return ('SliceFromTo', None, e)
            except Exception:
                self.pos = save2
                return ('SliceFromTo', None, None)
        else:
            e1 = self.parse_expr()
            if self.match('Colon'):
                save2 = self.pos
                try:
                    e2 = self.parse_expr()
                    return ('SliceFromTo', e1, e2)
                except Exception:
                    self.pos = save2
                    return ('SliceFromTo', e1, None)
            else:
                return ('SliceSingle', e1)

    def parse_func_call(self):
        name_tok = self.expect('Identifier')
        self.expect('OpenParen')
        args = []
        if self.current() and self.current().cls != 'CloseParen':
            args.append(self.parse_logic_expr())
            while self.match('Comma'):
                args.append(self.parse_logic_expr())
        self.expect('CloseParen')
        return ('CallFunc', name_tok.val, args)

    # Expression parsing with precedence
    def parse_logic_expr(self):
        left = self.parse_logic_term()
        while self.match('Or'):
            right = self.parse_logic_term()
            left = ('BinaryOp', 'OpOr', left, right)
        return left

    def parse_logic_term(self):
        left = self.parse_rel_expr()
        while self.match('And'):
            right = self.parse_rel_expr()
            left = ('BinaryOp', 'OpAnd', left, right)
        return left

    def parse_rel_expr(self):
        if self.match('Not'):
            sub = self.parse_rel_expr()
            return ('UnaryOp', 'OpNot', sub)
        save = self.pos
        try:
            left = self.parse_expr()
            rel_ops = ['Equal', 'NotEqual', 'LessEqual', 'GreaterEqual', 'Less', 'Greater', 'In']
            op = None
            for ro in rel_ops:
                if self.match(ro):
                    op = ro
                    break
            if op:
                right = self.parse_expr()
                return ('BinaryOp', op, left, right)
            self.pos = save
        except Exception:
            self.pos = save
        return self.parse_expr()

    def parse_expr(self):
        left = self.parse_term()
        while True:
            if self.match('Plus'):
                r = self.parse_term()
                left = ('BinaryOp', 'OpAdd', left, r)
            elif self.match('Minus'):
                r = self.parse_term()
                left = ('BinaryOp', 'OpSub', left, r)
            elif self.match('Cat'):
                r = self.parse_term()
                left = ('BinaryOp', 'OpCat', left, r)
            else:
                break
        return left

    def parse_term(self):
        left = self.parse_factor()
        while True:
            if self.match('Multiply'):
                r = self.parse_factor()
                left = ('BinaryOp', 'OpMul', left, r)
            elif self.match('Divide'):
                r = self.parse_factor()
                left = ('BinaryOp', 'OpDiv', left, r)
            elif self.match('Modulo'):
                r = self.parse_factor()
                left = ('BinaryOp', 'OpMod', left, r)
            elif self.match('MatrixMult'):
                r = self.parse_factor()
                left = ('BinaryOp', 'OpMatMul', left, r)
            else:
                break
        return left

    def parse_factor(self):
        u = self.parse_unary()
        if self.match('Power') or self.match('Caret'):
            r = self.parse_factor()
            return ('BinaryOp', 'OpPow', u, r)
        return u

    def parse_unary(self):
        if self.match('Minus'):
            return ('UnaryOp', 'OpNeg', self.parse_unary())
        if self.match('Plus'):
            return ('UnaryOp', 'OpPos', self.parse_unary())
        return self.parse_primary()

    def parse_primary(self):
        tok = self.current()
        if not tok:
            raise SyntaxError("Unexpected EOF in expr")

        if tok.cls == 'IntLiteral':
            self.pos += 1
            return ('IntLit', tok.val)
        if tok.cls == 'FloatLiteral':
            self.pos += 1
            return ('FloatLit', tok.val)
        if tok.cls == 'StringLiteral':
            self.pos += 1
            return ('StringLit', tok.val)
        if tok.cls == 'BoolLiteral':
            self.pos += 1
            return ('BoolLit', tok.val)

        if tok.cls == 'OpenParen':
            self.pos += 1
            e = self.parse_logic_expr()
            self.expect('CloseParen')
            return e

        if tok.cls == 'OpenBracket':
            self.pos += 1
            if self.match('CloseBracket'):
                return ('ListLit', [])
            # Could be list comprehension or list literal
            save = self.pos
            try:
                first = self.parse_logic_expr()
                if self.match('For'):
                    var_tok = self.expect('Identifier')
                    self.expect('In')
                    src = self.parse_expr()
                    cond = None
                    if self.match('If'):
                        cond = self.parse_logic_expr()
                    self.expect('CloseBracket')
                    return ('ListComp', first, var_tok.val, src, cond)
            except Exception:
                pass
            self.pos = save
            elems = [self.parse_logic_expr()]
            while self.match('Comma'):
                elems.append(self.parse_logic_expr())
            self.expect('CloseBracket')
            return ('ListLit', elems)

        if tok.cls == 'OpenBrace':
            self.pos += 1
            pairs = []
            if not self.match('CloseBrace'):
                k = self.expect('StringLiteral').val
                self.expect('Colon')
                v = self.parse_logic_expr()
                pairs.append((k, v))
                while self.match('Comma'):
                    k = self.expect('StringLiteral').val
                    self.expect('Colon')
                    v = self.parse_logic_expr()
                    pairs.append((k, v))
                self.expect('CloseBrace')
            return ('DictLit', pairs)

        if tok.cls == 'Identifier':
            # func_call or var
            save = self.pos
            try:
                fc = self.parse_func_call()
                return ('FuncCallExpr', fc)
            except Exception:
                self.pos = save
                v, _ = self.parse_var()
                return ('VarExpr', v)

        raise SyntaxError(f"Unexpected token in primary: {tok}")

# Parse quickSortV1
p1 = Parser(v1_toks)
ast1 = p1.parse_program()
print("\n=== quickSortV1 PARSED & SEMANTICALLY VALIDATED! ===")
print(f"Total top-level AST nodes: {len(ast1)}")
print("Symbol Table (escopo#nome) for quickSortV1:")
for s in p1.symtable:
    print(f"  {s[0]} -> {s[1]}")
v1_keys = {t1.val for t1, _ in p1.symtable}
assert "quicksort_1#lista" in v1_keys and "quicksort_1#pivo" in v1_keys
assert "lc_2#x" in v1_keys and "lc_3#x" in v1_keys
assert "global#entrada" in v1_keys and "global#intercala_1" in v1_keys
assert p1.symtable_lookup_scoped("x") is None, "List comprehension var 'x' leaked!"

# Parse quickSortV2
p2 = Parser(v2_toks)
ast2 = p2.parse_program()
print("\n=== quickSortV2 PARSED & SEMANTICALLY VALIDATED! ===")
print(f"Total top-level AST nodes: {len(ast2)}")
print("Symbol Table (escopo#nome) for quickSortV2:")
for s in p2.symtable:
    print(f"  {s[0]} -> {s[1]}")
v2_keys = {t1.val for t1, _ in p2.symtable}
assert "quicksort_2#arr" in v2_keys and "mergesort_2#arr" in v2_keys and "global#main" in v2_keys

# ==================== EDGE CASE UNIT TESTS ====================
print("\n=== RUNNING EDGE CASE TESTS ===")
test_cases = [
    ("Empty block", "def f() {} f();"),
    ("If-Else-If ladder", """
        x = 1;
        if (x < 0) {
            y = -1;
        } else if (x == 0) {
            y = 0;
        } else {
            y = 1;
        }
    """),
    ("Slices variants", """
        l = [1, 2, 3, 4, 5, 6];
        i = 0;
        a = l[:];
        b = l[1:];
        c = l[:5];
        d = l[1:5];
        e = l[i];
    """),
    ("Multidimensional Index", """
        mat = [[1, 2], [3, 4]];
        i = 0;
        j = 1;
        val = 99;
        x = mat[i, j];
        mat[i, j] = val;
    """),
    ("List comprehension without if", "items = [1, 2, 3]; res = [x * 2 for x in items];"),
    ("List comprehension with if", "items = [1, 2, 3]; res = [x for x in items if x % 2 == 0];"),
    ("Dictionary empty and with pairs", """
        d1 = {};
        d2 = {"key1": 10, "key2": "val"};
    """),
    ("Power operator", "z = 2 ** 3 ^ 4;"),
    ("Unary operators and precedence", "x = 3; y = 4; res = -x + +y * 2;"),
    ("Logical operators precedence", "a = true; b = false; c = true; d = false; flag = a and b or not c and d;"),
    ("Const variants (float, string, bool, id)", """
        const PI = 3.14;
        const MSG = "hello";
        const FLAG = true;
        const OTHER_FLAG = false;
        const REF = PI;
    """),
    ("Model with subprogram and assignment", """
        model Network {
            w = [[1, 2], [3, 4]];
            b = [0, 0];
            def forward(x) {
                return x @ w + b;
            }
        }
    """),
    ("Tensor matmul, string cat, modulo, assign ops", """
        a = [[1, 2]];
        b = [[3], [4]];
        c = a @ b;
        s = "hello " cat "world";
        rem = 10 % 3;
        w = 1;
        loss = 100;
        w *= 2;
        loss /= 10;
    """),
    ("Enum single member", "enum Single { OnlyOne }"),
    ("Return without expr", "def stop() { return; }"),
    ("While loop", "count = 5; while (count > 0) { count -= 1; }"),
    ("Consecutive list comprehensions isolated scope", """
        lista = [1, 2, 3, 6, 7];
        a = [x for x in lista if x < 5];
        b = [x for x in lista if x >= 5];
    """),
    ("Chained matrix multiplication, zeros, ones, reshape, dot", """
        m1 = zeros(2, 3);
        m2 = ones(3, 4);
        m3 = zeros(4, 2);
        res = (m1 @ m2) @ m3;
        r2 = reshape([[1, 2, 3, 4]], [2, 2]) @ [[1], [2]];
        d = dot([1, 2], [3, 4]);
    """),
    ("Built-in conversions and dict string indexing", """
        n = to_int("42") + 1;
        f = to_float(3) + 0.5;
        parts = split("a b", " ");
        cfg = {"lr": 0.01};
        v = cfg["lr"];
    """)
]

for name, code in test_cases:
    try:
        toks = scan(code)
        p = Parser(toks)
        ast = p.parse_program()
        if name == "Consecutive list comprehensions isolated scope":
            assert p.symtable_lookup_scoped("x") is None
            keys = {t1.val for t1, _ in p.symtable}
            assert "lc_1#x" in keys and "lc_2#x" in keys
        print(f"  PASS: {name} ({len(ast)} stmts, {len(p.symtable)} symbols)")
    except Exception as e:
        print(f"  FAIL: {name} -> {e}")
        sys.exit(1)

print("\n=== RUNNING NEGATIVE SYNTAX TESTS (MUST REJECT) ===")
negative_syntax_cases = [
    ("Empty enum definition", "enum Empty {}"),
    ("Const declaration with complex expression", "const BAD = 1 + 2;"),
    ("Empty assignment RHS", "x = ;"),
    ("Disallowed var keyword", "var x : int = 10;"),
    ("Unterminated block", "def f() { x = 1;"),
    ("Disallowed increment operator ++", "i++;")
]

for name, code in negative_syntax_cases:
    try:
        toks = scan(code)
        p = Parser(toks)
        ast = p.parse_program()
        print(f"  FAILED TO REJECT (Unexpected Success): {name}")
        sys.exit(1)
    except SyntaxError as e:
        print(f"  PASS (SyntaxError): {name} -> {e}")
    except Exception as e:
        print(f"  UNEXPECTED EXCEPTION TYPE ({type(e).__name__}): {name} -> {e}")
        sys.exit(1)

print("\n=== RUNNING NEGATIVE SEMANTIC TESTS (MUST REJECT) ===")
negative_semantic_cases = [
    ("Undeclared variable in expression", "y = nao_existe + 1;"),
    ("List comprehension var used outside", "l = [1, 2]; a = [x for x in l]; y = x;"),
    ("Compound assign on undeclared var", "z += 1;"),
    ("Reassign to const", "const PI = 3.14; PI = 3.15;"),
    ("Const initialized with mutable var", "a = 10; const C = a;"),
    ("Call undeclared function", "inexistente(1, 2);"),
    ("Function call with wrong arity", "def f(a, b) { return a; } f(1);"),
    ("Incompatible types in '+'", 'x = 10 + "texto";'),
    ("Non-boolean condition in if/while", "if (123) { x = 1; }"),
    ("Return outside function", "return 10;"),
    ("Duplicate const in same scope", "const DUP = 1; const DUP = 2;"),
    ("Division by zero literal", "x = 10 / 0;"),
    ("Division by zero constant", "const Z = 0; x = 10 / Z;"),
    ("Matrix dimension mismatch (2x3) @ (4x2)", "a = zeros(2, 3); b = ones(4, 2); c = a @ b;"),
    ("Chained matrix dimension mismatch (2x3)@(3x4)@(5x1)", "a = zeros(2, 3); b = ones(3, 4); c = zeros(5, 1); r = (a @ b) @ c;"),
    ("Matrix x vector dimension mismatch", "a = [[1, 2, 3], [4, 5, 6]]; v = [1, 2]; r = a @ v;"),
    ("Vector x matrix dimension mismatch", "v = [1, 2]; a = [[1], [2], [3]]; r = v @ a;"),
    ("Irregular matrix in '@'", "a = [[1, 2], [3]]; b = [[1], [2]]; c = a @ b;"),
    ("Non-numeric matrix elements in '@'", 'a = [["a", "b"]]; b = [["c"], ["d"]]; c = a @ b;'),
    ("Dot product dimension mismatch", "d = dot([1, 2], [3, 4, 5]);"),
    ("Reshape element count mismatch", "r = reshape(zeros(2, 3), [2, 2]);"),
    ("Zeros non-positive dimension", "m = zeros(0, 3);"),
    ("Indexing non-indexable scalar", "x = 10; y = x[0];"),
    ("Too many indices for 2D matrix", "m = [[1, 2], [3, 4]]; x = m[0, 1, 2];"),
    ("String immutability on indexed assignment", 's = "abc"; s[0] = "x";'),
    ("Invalid dict key type (int)", 'd = {"a": 1}; x = d[0];'),
    ("Invalid 'in' left operand on string", 'x = 1 in "abc";'),
    ("Invalid built-in argument type: len(42)", "x = len(42);"),
    ("Invalid built-in argument type: split(123, ...)", 'x = split(123, " ");'),
    ("Invalid boolean ordering: true < false", "x = true < false;"),
    ("Invalid logical operands: 1 and 2", "x = 1 and 2;"),
    ("Invalid unary not on int", "x = not 1;"),
    ("Invalid unary minus on string", 'x = -"abc";'),
    ("Compound assign type mismatch: str -= str", 's = "a"; s -= "b";'),
    ("Compound assign type mismatch: str += int", 's = "a"; s += 1;'),
    ("Duplicate function parameter", "def f(a, a) { return a; }")
]

for name, code in negative_semantic_cases:
    try:
        toks = scan(code)
        p = Parser(toks)
        ast = p.parse_program()
        print(f"  FAILED TO REJECT (Unexpected Success): {name}")
        sys.exit(1)
    except SemanticError as e:
        print(f"  PASS (SemanticError): {name} -> {e}")
    except Exception as e:
        print(f"  UNEXPECTED EXCEPTION TYPE ({type(e).__name__}): {name} -> {e}")
        sys.exit(1)

print("\nALL POSITIVE, NEGATIVE SYNTAX, AND NEGATIVE SEMANTIC TESTS PASSED!")


