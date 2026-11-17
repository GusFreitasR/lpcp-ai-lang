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

# Parser implementation mimicking Parsec combinators
class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.pos = 0
        self.symtable = []

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

    def symtable_lookup(self, tok):
        for t1, t2 in self.symtable:
            if t1.val == tok.val:
                return (t1, t2)
        return None

    def symtable_insert(self, t1, t2):
        self.symtable.append((t1, t2))

    def symtable_update(self, t1, t2):
        for idx, (tok1, tok2) in enumerate(self.symtable):
            if tok1.val == t1.val:
                self.symtable[idx] = (t1, t2)
                return
        raise Exception("variable not found")

    def symtable_remove(self, t1, t2):
        for idx, (tok1, tok2) in enumerate(self.symtable):
            if tok1.val == t1.val:
                del self.symtable[idx]
                return
        raise Exception("variable not found")

    def symtable_insert_or_update(self, t1, t2):
        if self.symtable_lookup(t1):
            self.symtable_update(t1, t2)
        else:
            self.symtable_insert(t1, t2)

    def parse_program(self):
        stmts = []
        while self.current() is not None:
            stmts.append(self.parse_stmt())
        return stmts

    def parse_block(self):
        self.expect('OpenBrace')
        stmts = []
        while self.current() and self.current().cls != 'CloseBrace':
            stmts.append(self.parse_stmt())
        self.expect('CloseBrace')
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
            # Could be assign or func_call
            save = self.pos
            try:
                assign = self.parse_assign()
                self.expect('SemiColon')
                return ('AssignStmt', assign)
            except Exception:
                self.pos = save
                fc = self.parse_func_call()
                self.expect('SemiColon')
                return ('FuncCallStmt', fc)

    def parse_const(self):
        const_tok = self.expect('Const')
        name_tok = self.expect('Identifier')
        self.expect('Assign')
        e, val_tok = self.parse_const_val()
        self.expect('SemiColon')
        self.symtable_insert(name_tok, val_tok)
        return ('ConstDecl', name_tok.val, e)

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
        body = self.parse_block()
        self.symtable_insert(name_tok, model_tok)
        return ('ModelDef', name_tok.val, body)

    def parse_enum(self):
        enum_tok = self.expect('Enum')
        name_tok = self.expect('Identifier')
        self.expect('OpenBrace')
        members = [self.expect('Identifier')]
        while self.match('Comma'):
            members.append(self.expect('Identifier'))
        self.expect('CloseBrace')
        self.symtable_insert(name_tok, enum_tok)
        for m in members:
            self.symtable_insert(m, enum_tok)
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

        self.symtable_insert(name_tok, def_tok)
        for pt in param_toks:
            self.symtable_insert(pt, def_tok)

        body = self.parse_block()
        return ('FuncDef', name_tok.val, [pt.val for pt in param_toks], body)

    def parse_if(self):
        self.expect('If')
        self.expect('OpenParen')
        cond = self.parse_logic_expr()
        self.expect('CloseParen')
        then_b = self.parse_block()
        else_b = None
        if self.match('Else'):
            if self.current() and self.current().cls == 'If':
                else_b = [self.parse_if()]
            else:
                else_b = self.parse_block()
        return ('IfStmt', cond, then_b, else_b)

    def parse_while(self):
        self.expect('While')
        self.expect('OpenParen')
        cond = self.parse_logic_expr()
        self.expect('CloseParen')
        body = self.parse_block()
        return ('WhileStmt', cond, body)

    def parse_for(self):
        self.expect('For')
        self.expect('OpenParen')
        save = self.pos
        # Try traditional for (assign ; logic ; assign)
        try:
            init = self.parse_assign()
            self.expect('SemiColon')
            cond = self.parse_logic_expr()
            self.expect('SemiColon')
            step = self.parse_assign()
            self.expect('CloseParen')
            body = self.parse_block()
            return ('ForStmt', init, cond, step, body)
        except Exception:
            self.pos = save
            var_tok = self.expect('Identifier')
            in_tok = self.expect('In')
            src = self.parse_expr()
            self.expect('CloseParen')
            self.symtable_insert_or_update(var_tok, in_tok)
            body = self.parse_block()
            return ('ForInStmt', var_tok.val, src, body)

    def parse_return(self):
        self.expect('Return')
        val = None
        if self.current() and self.current().cls != 'SemiColon':
            val = self.parse_logic_expr()
        self.expect('SemiColon')
        return ('ReturnStmt', val)

    def parse_print(self):
        if self.match('Print') is None:
            id_t = self.expect('Identifier')
            assert id_t.val == 'print'
        self.expect('OpenParen')
        e = self.parse_logic_expr()
        self.expect('CloseParen')
        self.expect('SemiColon')
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
        val_tok = Token(base_tok.pos, op)
        self.symtable_insert_or_update(base_tok, val_tok)
        return ('Assign', v, op, e)

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
            args.append(self.parse_expr())
            while self.match('Comma'):
                args.append(self.parse_expr())
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
                first = self.parse_expr()
                if self.match('For'):
                    var_tok = self.expect('Identifier')
                    in_tok = self.expect('In')
                    src = self.parse_expr()
                    cond = None
                    if self.match('If'):
                        cond = self.parse_logic_expr()
                    self.expect('CloseBracket')
                    self.symtable_insert_or_update(var_tok, in_tok)
                    return ('ListComp', first, var_tok.val, src, cond)
            except Exception:
                pass
            self.pos = save
            elems = [self.parse_expr()]
            while self.match('Comma'):
                elems.append(self.parse_expr())
            self.expect('CloseBracket')
            return ('ListLit', elems)

        if tok.cls == 'OpenBrace':
            self.pos += 1
            pairs = []
            if not self.match('CloseBrace'):
                k = self.expect('StringLiteral').val
                self.expect('Colon')
                v = self.parse_expr()
                pairs.append((k, v))
                while self.match('Comma'):
                    k = self.expect('StringLiteral').val
                    self.expect('Colon')
                    v = self.parse_expr()
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
print("\n=== quickSortV1 PARSED SUCCESSFULLY! ===")
print(f"Total top-level AST nodes: {len(ast1)}")
print("Symbol Table for quickSortV1:")
for s in p1.symtable:
    print(f"  {s[0]} -> {s[1]}")

# Parse quickSortV2
p2 = Parser(v2_toks)
ast2 = p2.parse_program()
print("\n=== quickSortV2 PARSED SUCCESSFULLY! ===")
print(f"Total top-level AST nodes: {len(ast2)}")
print("Symbol Table for quickSortV2:")
for s in p2.symtable:
    print(f"  {s[0]} -> {s[1]}")

# ==================== EDGE CASE UNIT TESTS ====================
print("\n=== RUNNING EDGE CASE TESTS ===")
test_cases = [
    ("Empty block", "def f() {} f();"),
    ("If-Else-If ladder", """
        if (x < 0) {
            y = -1;
        } else if (x == 0) {
            y = 0;
        } else {
            y = 1;
        }
    """),
    ("Slices variants", """
        a = l[:];
        b = l[1:];
        c = l[:5];
        d = l[1:5];
        e = l[i];
    """),
    ("Multidimensional Index", """
        x = mat[i, j];
        mat[i, j] = val;
    """),
    ("List comprehension without if", "res = [x * 2 for x in items];"),
    ("List comprehension with if", "res = [x for x in items if x % 2 == 0];"),
    ("Dictionary empty and with pairs", """
        d1 = {};
        d2 = {"key1": 10, "key2": "val"};
    """),
    ("Power operator", "z = 2 ** 3 ^ 4;"),
    ("Unary operators and precedence", "res = -x + +y * 2;"),
    ("Logical operators precedence", "flag = a and b or not c and d;"),
    ("Const variants (float, string, bool, id)", """
        const PI = 3.14;
        const MSG = "hello";
        const FLAG = true;
        const OTHER_FLAG = false;
        const REF = PI;
    """),
    ("Model with subprogram and assignment", """
        model Network {
            def forward(x) {
                return x @ w + b;
            }
        }
    """),
    ("Tensor matmul, string cat, modulo, assign ops", """
        c = a @ b;
        s = "hello " cat "world";
        rem = 10 % 3;
        w *= 2;
        loss /= 10;
    """),
    ("Enum single member", "enum Single { OnlyOne }"),
    ("Return without expr", "def stop() { return; }"),
    ("While loop", "while (count > 0) { count -= 1; }")
]

for name, code in test_cases:
    try:
        toks = scan(code)
        p = Parser(toks)
        ast = p.parse_program()
        print(f"  PASS: {name} ({len(ast)} stmts, {len(p.symtable)} symbols)")
    except Exception as e:
        print(f"  FAIL: {name} -> {e}")
        sys.exit(1)

print("\n=== RUNNING NEGATIVE SYNTAX TESTS (MUST REJECT) ===")
negative_cases = [
    ("Empty enum definition", "enum Empty {}"),
    ("Const declaration with complex expression", "const BAD = 1 + 2;"),
    ("Empty assignment RHS", "x = ;"),
    ("Disallowed var keyword", "var x : int = 10;"),
    ("Unterminated block", "def f() { x = 1;"),
    ("Disallowed increment operator ++", "i++;")
]

for name, code in negative_cases:
    try:
        toks = scan(code)
        p = Parser(toks)
        ast = p.parse_program()
        print(f"  FAILED TO REJECT (Unexpected Success): {name}")
        sys.exit(1)
    except Exception as e:
        print(f"  PASS (Correctly Rejected): {name} -> {type(e).__name__}: {e}")

print("\nALL POSITIVE AND NEGATIVE EDGE CASE TESTS PASSED!")


