################################### IMPORTS ###############################################

from arrows import *

################################### CONSTANTS ###############################################

Digits = '0123456789'

################################### ERRORS ##################################################

class Error:
    def __init__(self, pos_start, pos_end, error_name, details):
        self.pos_start = pos_start
        self.pos_end = pos_end
        self.error_name = error_name
        self.details = details

    def __str__(self):
        result = f'{self.error_name}: {self.details}\n'
        result += f'File {self.pos_start.fn}, line {self.pos_start.line + 1}'
        result += '\n\n' + arrows(self.pos_start.ftxt, self.pos_start, self.pos_end)
        return result

class IllegalCharError(Error):
    def __init__(self, pos_start, pos_end, details):
        super().__init__(pos_start, pos_end, 'Illegal Character Error', details)

class IllegalFloatError(Error):
    def __init__(self, pos_start, pos_end, details):
        super().__init__(pos_start, pos_end, 'Illegal Floating Point Error', details)

class InvalidSyntaxError(Error):
    def __init__(self, pos_start, pos_end, details=''):
        super().__init__(pos_start, pos_end, 'Invalid Syntax', details)

class RTError(Error):
    def __init__(self, pos_start, pos_end, context, details=''):
        super().__init__(pos_start, pos_end, 'Runtime Error', details)
        self.context = context

    def generatetraceback(self):
        result = ''
        pos = self.pos_start
        ctx = self.context

        while ctx:
            result = f' File {pos.fn}, line {str(pos.line + 1)}, in {ctx.display_name}\n' + result
            pos = ctx.parent_entry_pos
            ctx = ctx.parent

        return 'Traceback (most recent call last):\n' + result

    def __str__(self):
        result = self.generatetraceback()
        result += f'{self.error_name}: {self.details}\n'
        result += '\n\n' + arrows(self.pos_start.ftxt, self.pos_start, self.pos_end)
        return result

################################### POSITIONS ################################################

class Position:
    def __init__(self, index, line, column, fn, ftxt):
        self.index = index
        self.line = line
        self.column = column
        self.fn = fn
        self.ftxt = ftxt

    def advance(self, current_char=None):
        self.index += 1
        self.column += 1
        if current_char == '\n':
            self.line += 1
            self.column = 0
        return self

    def copy(self):
        return Position(self.index, self.line, self.column, self.fn, self.ftxt)

################################### TOKENS ################################################

T_INT = 'INT'
T_FLOAT = 'FLOAT'
T_PLUS = 'PLUS'
T_MINUS = 'MINUS'
T_MUL = 'MUL'
T_DIV = 'DIV'
T_LPAREN = 'LPAREN'
T_RPAREN = 'RPAREN'
T_EOF = 'EOF'

class Token:
    def __init__(self, type_, value=None, pos_start=None, pos_end=None):
        self.type = type_
        self.value = value

        if pos_start:
            self.pos_start = pos_start.copy()
            self.pos_end = pos_start.copy()
            self.pos_end.advance()
        if pos_end:
            self.pos_end = pos_end

    def __repr__(self):
        if self.value is not None:
            return f'{self.type}:{self.value}'
        return f'{self.type}'

################################### LEXER ################################################

class Lexer:
    def __init__(self, fn, text):
        self.fn = fn
        self.text = text
        self.pos = Position(-1, 0, -1, fn, text)
        self.current_char = None
        self.advance()

    def advance(self):
        self.pos.advance(self.current_char)
        self.current_char = self.text[self.pos.index] if self.pos.index < len(self.text) else None

    def make_tokens(self):
        tokens = []
        while self.current_char is not None:
            if self.current_char in ' \t':
                self.advance()
            elif self.current_char == '+':
                tokens.append(Token(T_PLUS, pos_start=self.pos))
                self.advance()
            elif self.current_char == '-':
                tokens.append(Token(T_MINUS, pos_start=self.pos))
                self.advance()
            elif self.current_char == '*':
                tokens.append(Token(T_MUL, pos_start=self.pos))
                self.advance()
            elif self.current_char == '/':
                tokens.append(Token(T_DIV, pos_start=self.pos))
                self.advance()
            elif self.current_char == ')':
                tokens.append(Token(T_RPAREN, pos_start=self.pos))
                self.advance()
            elif self.current_char == '(':
                tokens.append(Token(T_LPAREN, pos_start=self.pos))
                self.advance()
            elif self.current_char in Digits:
                pos_start = self.pos.copy()
                num_token = self.identify_num()
                if isinstance(num_token, str):
                    return [], IllegalFloatError(pos_start, self.pos, f"Invalid number: {num_token}")
                tokens.append(num_token)
            else:
                pos_start = self.pos.copy()
                char = self.current_char
                self.advance()
                return [], IllegalCharError(pos_start, self.pos, f"'{char}'")
        tokens.append(Token(T_EOF, pos_start=self.pos))
        return tokens, None

    def identify_num(self):
        dot_count = 0
        num = ''
        pos_start = self.pos.copy()
        while self.current_char is not None and self.current_char in Digits + '.':
            if self.current_char == '.':
                dot_count += 1
            num += self.current_char
            self.advance()
        if dot_count == 0:
            return Token(T_INT, int(num), pos_start, self.pos)
        elif dot_count == 1:
            return Token(T_FLOAT, float(num), pos_start, self.pos)
        else:
            return num

################################### NODES ################################################

class NumberNode:
    def __init__(self, tok):
        self.tok = tok
        self.pos_start = self.tok.pos_start
        self.pos_end = self.tok.pos_end

    def __repr__(self):
        return f'{self.tok}'

class BinOpNode:
    def __init__(self, left_node, op_tok, right_node):
        self.left_node = left_node
        self.op_tok = op_tok
        self.right_node = right_node
        self.pos_start = self.left_node.pos_start
        self.pos_end = self.right_node.pos_end

    def __repr__(self):
        return f'({self.left_node}, {self.op_tok}, {self.right_node})'

class UnaryOpNode:
    def __init__(self, op_tok, node):
        self.op_tok = op_tok
        self.node = node
        self.pos_start = self.op_tok.pos_start
        self.pos_end = self.op_tok.pos_end

    def __repr__(self):
        return f'({self.op_tok},{self.node})'

################################### PARSE RESULT ################################################

class ParseResult:
    def __init__(self):
        self.node = None
        self.error = None

    def register(self, res):
        if isinstance(res, ParseResult):
            if res.error:
                self.error = res.error
            return res.node
        return res

    def success(self, node):
        self.node = node
        return self

    def failure(self, error):
        self.error = error
        return self

################################### PARSER ################################################

class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.token_index = -1
        self.current_token = None
        self.advance()

    def advance(self):
        self.token_index += 1
        if self.token_index < len(self.tokens):
            self.current_token = self.tokens[self.token_index]
        return self.current_token

    def parse(self):
        res = self.expression()
        if not res.error and self.current_token.type != T_EOF:
            return res.failure(InvalidSyntaxError(
                self.current_token.pos_start, self.current_token.pos_end,
                "Expected '+','-','*' or '/'"
            ))
        return res

    def factor(self):
        res = ParseResult()
        tok = self.current_token

        if tok.type in (T_PLUS, T_MINUS):
            res.register(self.advance())
            factor = res.register(self.factor())
            if res.error: return res
            return res.success(UnaryOpNode(tok, factor))

        elif tok.type in [T_INT, T_FLOAT]:
            res.register(self.advance())
            return res.success(NumberNode(tok))

        elif tok.type == T_LPAREN:
            res.register(self.advance())
            expr = res.register(self.expression())
            if res.error: return res

            if self.current_token.type == T_RPAREN:
                res.register(self.advance())
                return res.success(expr)
            else:
                return res.failure(InvalidSyntaxError(
                    self.current_token.pos_start, self.current_token.pos_end,
                    "Expected ')'"
                ))

        return res.failure(InvalidSyntaxError(
            tok.pos_start, tok.pos_end,
            "Expected int, float, '+', '-', or '('"
        ))

    def term(self):
        return self.binary_operation(self.factor, (T_MUL, T_DIV))

    def expression(self):
        return self.binary_operation(self.term, (T_PLUS, T_MINUS))

    def binary_operation(self, func, ops):
        res = ParseResult()
        left = res.register(func())
        if res.error: return res

        while self.current_token.type in ops:
            op_tok = self.current_token
            res.register(self.advance())
            right = res.register(func())
            if res.error: return res
            left = BinOpNode(left, op_tok, right)

        return res.success(left)

################################### RUNTIME ################################################

class Number:
    def __init__(self, value):
        self.value = value
        self.set_pos()
        self.set_context()

    def set_pos(self, pos_start=None, pos_end=None):
        self.pos_start = pos_start
        self.pos_end = pos_end
        return self

    def set_context(self, context=None):
        self.context = context
        return self

    def added_to(self, other):
        if isinstance(other, Number):
            return Number(self.value + other.value).set_context(self.context), None

    def subbed_by(self, other):
        if isinstance(other, Number):
            return Number(self.value - other.value).set_context(self.context), None

    def multed_by(self, other):
        if isinstance(other, Number):
            return Number(self.value * other.value).set_context(self.context), None

    def dived_by(self, other):
        if isinstance(other, Number):
            if other.value == 0:
                return None, RTError(
                    other.pos_start, other.pos_end,
                    self.context, "Division by zero"
                )
            return Number(self.value / other.value).set_context(self.context), None

    def __repr__(self):
        return str(self.value)

class Context:
    def __init__(self, display_name, parent=None, parent_entry_pos=None):
        self.display_name = display_name
        self.parent = parent
        self.parent_entry_pos = parent_entry_pos

class Interpreter:
    def visit(self, node, context):
        method_name = f'visit_{type(node).__name__}'
        method = getattr(self, method_name, self.no_visit_method)
        return method(node, context)

    def no_visit_method(self, node, context):
        raise Exception(f'No visit_{type(node).__name__} method defined')

    def visit_NumberNode(self, node, context):
        return Number(node.tok.value).set_context(context).set_pos(node.pos_start, node.pos_end), None

    def visit_BinOpNode(self, node, context):
        left, error = self.visit(node.left_node, context)
        if error: return None, error

        right, error = self.visit(node.right_node, context)
        if error: return None, error

        if node.op_tok.type == T_PLUS:
            result, error = left.added_to(right)
        elif node.op_tok.type == T_MINUS:
            result, error = left.subbed_by(right)
        elif node.op_tok.type == T_MUL:
            result, error = left.multed_by(right)
        elif node.op_tok.type == T_DIV:
            result, error = left.dived_by(right)

        if error: return None, error
        return result.set_pos(node.pos_start, node.pos_end), None

    def visit_UnaryOpNode(self, node, context):
        number, error = self.visit(node.node, context)
        if error: return None, error

        if node.op_tok.type == T_MINUS:
            number, error = number.multed_by(Number(-1))
        if error: return None, error
        return number.set_pos(node.pos_start, node.pos_end), None

################################### RUN ################################################

def run(fn, text):
    lexer = Lexer(fn, text)
    tokens, error = lexer.make_tokens()
    if error: return None, error

    parser = Parser(tokens)
    ast = parser.parse()
    if ast.error: return None, ast.error

    interpreter = Interpreter()
    context = Context('<program>')
    result, error = interpreter.visit(ast.node, context)

    return result, error
