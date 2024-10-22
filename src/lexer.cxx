#include <cstdint>
#include <cstdlib>
#include <iostream>
#include <vector>
#include <unordered_map>
#include <fstream>
#include <ostream>
#include <sstream>

#include "lexer.hxx"
#include "token.hxx"
#include "grammar.hxx"
#include "common.hxx"

lexer::t::t() : hd(nullptr), tl(nullptr) {}

void lexer::dump(lexer::t &lexer) {
    auto it = lexer.hd;
    while (it) {
        std::cout << "{";
        std::cout << "LX: " << it->lx << ", ";
        std::cout << "TY: " << token::type_to_cxxstr(it->ty) << ", ";
        std::cout << "ROW: " << it->row << ", ";
        std::cout << "COL: " << it->col << ", ";
        std::cout << "FP: " << it->fp << "}" << std::endl;
        it = it->next;
    }
}

token::t *lexer::peek(lexer::t &lexer) {
    return lexer.hd ? lexer.hd.get() : nullptr;
}

sh_ptr<token::t> lexer::next(lexer::t &lexer) {
    if (!lexer.hd)
        return nullptr;

    auto t = std::move(lexer.hd);
    lexer.hd = t->next;

    if (!lexer.hd)
        lexer.tl = nullptr;

    return t;
}

void lexer::discard(lexer::t &lexer) {
    if (lexer.hd)
        lexer.hd = lexer.hd->next;
}

void lexer::append(lexer::t &lexer, sh_ptr<token::t> tok) {
    if(!lexer.hd) {
        lexer.hd = tok;
        lexer.tl = lexer.hd.get();
        return;
    }
    lexer.tl->next = tok;
    lexer.tl = lexer.tl->next.get();
}

static bool is_keyword(const str &lx, const vec<str> &keywords) {
    for (auto &s : keywords) {
        if (s == lx)
            return true;
    }
    return false;
}

static bool is_primitive_type(const str &lx, const vec<str> &primitives) {
    for (auto &s : primitives)
        if (s == lx)
            return true;
    return false;
}

static str parse_str(const str &s, size_t &sz_actual) {
    str buf = "";

    for (size_t i = 0, esc = 0; s[i]; ++i) {
        if (s[i] == '"' && !esc) {
            break;
        }

        if (s[i] == '\\') {
            esc = 1;
        }
        else {
            esc = 0;
        }
        buf += s[i];
    }
    sz_actual = buf.size();

    str cleaned = "";
    for (size_t i = 0; i < buf.size(); ++i) {
        if (i < buf.size()-1 && buf[i] == '\\' && buf[i+1] == 'n') {
            cleaned += '\n';
            ++i;
        }
        else
            cleaned += buf[i];
    }

    return cleaned;
}

str lexer::file_to_str(const str &file_path) {
    std::ifstream file(file_path);

    if (!file) {
        std::cerr << "Error: Could not open file " << file_path << std::endl;
        std::exit(EXIT_FAILURE);
    }

    std::ostringstream ss;
    ss << file.rdbuf();

    return ss.str();
}

lexer::t lexer::lex(str &src, str fp) {
    lexer::t lexer;
    std::vector<str> keywords = COMMON_SCR_KEYWORDS;
    std::vector<str> primitive_types = COMMON_SCR_PRIMITIVE_TYPES;
    std::unordered_map<str, token::type> symtbl = {
        {"+", token::type::Plus},
        {"-", token::type::Minus},
        {"*", token::type::Asterisk},
        {"/", token::type::Forwardslash},
        {"%", token::type::Percent},
        {"=", token::type::Equals},
        {">", token::type::Greaterthan},
        {"<", token::type::Lessthan},
        {"==", token::type::Double_Equals},
        {">=", token::type::Greaterthan_Equals},
        {"<=", token::type::Lessthan_Equals},
        {"!=", token::type::Bang_Equals},
        {"(", token::type::LParen},
        {")", token::type::RParen},
        {"{", token::type::LBrace},
        {"}", token::type::RBrace},
        {":", token::type::Colon},
        {";", token::type::Semicolon},
        {",", token::type::Comma},
    };

    unsigned r = 1, c = 0;
    size_t i = 0, debug_last = 1e10;
    size_t n = src.size();
    while (i < n) {
        if (i == debug_last) {
            std::cerr << "failed to lex at: " << src.substr(i) << std::endl;
            std::exit(EXIT_FAILURE);
        }
        debug_last = i;

        // Comments
        if (i < n-1 && src[i] == '-' && src[i+1] == '-') {
            while (src[i] != '\n')
                ++c, ++i;
        }

        // Ignorable
        else if (src[i] == '\n' || src[i] == '\r') {
            c = 0;
            ++r, ++i;
        }

        // Ignorable
        else if (src[i] == '\t' || src[i] == ' ') {
            ++c, ++i;
        }

        // Identifiers keywords, or primitive types
        else if (src[i] == '_' || std::isalpha(src[i])) {
            str buf = "";
            while (src[i] == '_' || isalnum(src[i]))
                buf += src[i++];

            sh_ptr<token::t> tok = nullptr;

            if (is_keyword(buf, keywords))
                tok = std::make_shared<token::t>(buf, token::type::Keyword, r, c, fp);
            else if (is_primitive_type(buf, primitive_types))
                tok = std::make_shared<token::t>(buf, token::type::Type, r, c, fp);
            else
                tok = std::make_shared<token::t>(buf, token::type::Ident, r, c, fp);

            lexer::append(lexer, std::move(tok));
        }

        // Numbers
        else if (isdigit(src[i])) {
            str num = "";
            while(isdigit(src[i]))
                num += src[i++];

            auto tok = std::make_shared<token::t>(num, token::type::Intlit, r, c, fp);
            lexer::append(lexer, std::move(tok));
        }

        // Strings
        else if (src[i] == '"') {
            size_t sz_actual = 0;
            str strlit = parse_str(&src[i+1], sz_actual);

            auto tok = std::make_shared<token::t>(strlit, token::type::Strlit, r, c, fp);
            lexer::append(lexer, std::move(tok));

            i += sz_actual + 1 + 1;
            c += sz_actual + 1 + 1;
        }

        // Symbols
        else {
            std::string buf = "";
            while (src[i] && !isalnum(src[i]) && src[i] != '_')
                buf += src[i++];
            while (!buf.empty()) {
                auto it = symtbl.find(buf);
                if (it != symtbl.end()) {
                    if (buf == "." && src[i] && isdigit(src[i])) {
                        std::string digit = "";
                        while (isdigit(src[i]))
                            digit += src[i++];
                        auto tok = std::make_shared<token::t>(buf+digit, token::type::Floatlit, r, c, fp);
                        lexer::append(lexer, std::move(tok));
                        c += digit.size()+1;
                    }
                    else {
                        auto tok = std::make_shared<token::t>(buf, (*it).second, r, c, fp);
                        lexer::append(lexer, std::move(tok));
                    }
                    break;
                }
                else {
                    buf.pop_back();
                    --i;
                }
            }
        }
    }

    return lexer;
}

