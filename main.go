// lsp_types.go
package main

import (
	"context"
	"encoding/json"
	"fmt"
	"github.com/sasogeek/simple/compiler/lexer"
	"github.com/sasogeek/simple/compiler/parser"
	"github.com/sourcegraph/jsonrpc2"
	"log"
	"os"
)

type Position struct {
	Line      int `json:"line"`
	Character int `json:"character"`
}

type Range struct {
	Start Position `json:"start"`
	End   Position `json:"end"`
}

type Location struct {
	URI   string `json:"uri"`
	Range Range  `json:"range"`
}

type Diagnostic struct {
	Range    Range  `json:"range"`
	Severity int    `json:"severity"`
	Message  string `json:"message"`
}

type TextDocumentItem struct {
	URI        string `json:"uri"`
	LanguageID string `json:"languageId"`
	Version    int    `json:"version"`
	Text       string `json:"text"`
}

type TextDocumentIdentifier struct {
	URI string `json:"uri"`
}

type VersionedTextDocumentIdentifier struct {
	URI     string `json:"uri"`
	Version int    `json:"version"`
}

type DidOpenTextDocumentParams struct {
	TextDocument TextDocumentItem `json:"textDocument"`
}

type DidChangeTextDocumentParams struct {
	TextDocument   VersionedTextDocumentIdentifier  `json:"textDocument"`
	ContentChanges []TextDocumentContentChangeEvent `json:"contentChanges"`
}

type TextDocumentContentChangeEvent struct {
	Text string `json:"text"`
}

type CompletionItem struct {
	Label string `json:"label"`
	Kind  int    `json:"kind,omitempty"`
}

type CompletionParams struct {
	TextDocument TextDocumentIdentifier `json:"textDocument"`
	Position     Position               `json:"position"`
}

type Hover struct {
	Contents string `json:"contents"`
	Range    *Range `json:"range,omitempty"`
}

type HoverParams struct {
	TextDocument TextDocumentIdentifier `json:"textDocument"`
	Position     Position               `json:"position"`
}

type DefinitionParams struct {
	TextDocument TextDocumentIdentifier `json:"textDocument"`
	Position     Position               `json:"position"`
}

type InitializeParams struct {
	ProcessID int    `json:"processId"`
	RootURI   string `json:"rootUri"`
}

type InitializeResult struct {
	Capabilities ServerCapabilities `json:"capabilities"`
}

type ServerCapabilities struct {
	TextDocumentSync   int                `json:"textDocumentSync"`
	CompletionProvider *CompletionOptions `json:"completionProvider,omitempty"`
	DefinitionProvider bool               `json:"definitionProvider"`
	HoverProvider      bool               `json:"hoverProvider"`
}

type CompletionOptions struct {
	ResolveProvider   bool     `json:"resolveProvider"`
	TriggerCharacters []string `json:"triggerCharacters,omitempty"`
}

type Server struct {
	conn      *jsonrpc2.Conn
	documents map[string]*Document
}

type Document struct {
	Text    string
	Lexer   *lexer.Lexer
	Parser  *parser.Parser
	AST     *parser.Program
	Symbols map[string]*parser.Identifier
}

// server.go

func (s *Server) handle(ctx context.Context, conn *jsonrpc2.Conn, req *jsonrpc2.Request) (interface{}, error) {
	switch req.Method {
	case "initialize":
		return s.handleInitialize(ctx, req)
	case "textDocument/didOpen":
		return s.handleDidOpen(ctx, req)
	case "textDocument/didChange":
		return s.handleDidChange(ctx, req)
	case "textDocument/completion":
		return s.handleCompletion(ctx, req)
	case "textDocument/hover":
		return s.handleHover(ctx, req)
	case "textDocument/definition":
		return s.handleDefinition(ctx, req)
	default:
		// Method not implemented
		return nil, nil
	}
}

func (s *Server) handleInitialize(ctx context.Context, req *jsonrpc2.Request) (interface{}, error) {
	var params InitializeParams
	if err := json.Unmarshal(*req.Params, &params); err != nil {
		return nil, err
	}

	capabilities := ServerCapabilities{
		TextDocumentSync:   1, // Full synchronization
		CompletionProvider: &CompletionOptions{TriggerCharacters: []string{"."}},
		DefinitionProvider: true,
		HoverProvider:      true,
	}

	result := InitializeResult{
		Capabilities: capabilities,
	}

	return result, nil
}

func (s *Server) handleDidOpen(ctx context.Context, req *jsonrpc2.Request) (interface{}, error) {
	var params DidOpenTextDocumentParams
	if err := json.Unmarshal(*req.Params, &params); err != nil {
		return nil, err
	}

	doc := &Document{
		Text: params.TextDocument.Text,
	}
	doc.Lexer = lexer.NewLexer(doc.Text)
	doc.Parser = parser.NewParser(doc.Lexer)
	doc.AST = doc.Parser.ParseProgram()

	// Populate the symbol table
	collectSymbols(doc.AST, doc.Symbols)

	s.documents[params.TextDocument.URI] = doc

	// Send diagnostics if there are errors
	s.sendDiagnostics(params.TextDocument.URI, doc.Parser.Errors())

	return nil, nil
}

func (s *Server) handleDidChange(ctx context.Context, req *jsonrpc2.Request) (interface{}, error) {
	var params DidChangeTextDocumentParams
	if err := json.Unmarshal(*req.Params, &params); err != nil {
		return nil, err
	}

	// For simplicity, we assume full text synchronization
	text := params.ContentChanges[0].Text

	doc := &Document{
		Text: text,
	}
	doc.Lexer = lexer.NewLexer(doc.Text)
	doc.Parser = parser.NewParser(doc.Lexer)
	doc.AST = doc.Parser.ParseProgram()

	// Populate the symbol table
	collectSymbols(doc.AST, doc.Symbols)

	s.documents[params.TextDocument.URI] = doc

	// Send diagnostics if there are errors
	s.sendDiagnostics(params.TextDocument.URI, doc.Parser.Errors())

	return nil, nil
}

func collectSymbols(ast *parser.Program, symbols map[string]*parser.Identifier) {
	parser.Inspect(ast, func(node parser.Node) bool {
		switch n := node.(type) {
		case *parser.FunctionLiteral:
			if n.Name != nil {
				symbols[n.Name.Value] = n.Name
			}
		}
		return true
	})
}

func (s *Server) sendDiagnostics(uri string, errors []string) {
	diagnostics := []Diagnostic{}
	for _, errMsg := range errors {
		// Example error format: "expected X, got Y instead (Line 3, Column 15)"
		line, column := parseErrorPosition(errMsg)
		if line < 0 || column < 0 {
			continue // Skip errors without position information
		}
		diag := Diagnostic{
			Range: Range{
				Start: Position{Line: line, Character: column},
				End:   Position{Line: line, Character: column + 1},
			},
			Severity: 1, // Error
			Message:  errMsg,
		}
		diagnostics = append(diagnostics, diag)
	}

	params := struct {
		URI         string       `json:"uri"`
		Diagnostics []Diagnostic `json:"diagnostics"`
	}{
		URI:         uri,
		Diagnostics: diagnostics,
	}

	s.conn.Notify(context.Background(), "textDocument/publishDiagnostics", params)
}

func parseErrorPosition(errMsg string) (int, int) {
	var line, column int
	// Example error format: "expected X, got Y instead (Line 3, Column 15)"
	n, err := fmt.Sscanf(errMsg, "expected %*s, got %*s instead (Line %d, Column %d)", &line, &column)
	if err != nil || n != 2 {
		// Handle different error message formats or return invalid positions
		return -1, -1
	}
	return line - 1, column - 1 // Convert to zero-based indexing
}

func (s *Server) handleHover(ctx context.Context, req *jsonrpc2.Request) (interface{}, error) {
	var params HoverParams
	if err := json.Unmarshal(*req.Params, &params); err != nil {
		return nil, err
	}

	doc := s.documents[params.TextDocument.URI]
	if doc == nil {
		return nil, nil
	}

	// Find the token at the given position
	token := findTokenAtPosition(doc.Lexer, params.Position)
	if token == nil {
		return nil, nil
	}

	// Retrieve symbol information
	symbol, exists := doc.Symbols[token.Literal]
	if !exists {
		return nil, nil
	}

	// Provide hover information based on symbol type
	content := fmt.Sprintf("**Function** `%s`\n\nDefined at Line %d, Column %d", symbol.Value, symbol.Token.Line, symbol.Token.Column)

	hover := Hover{
		Contents: content,
	}

	return hover, nil
}

func findTokenAtPosition(l *lexer.Lexer, position Position) *lexer.Token {
	tokens := lexAllTokens(l)
	for _, tok := range tokens {
		if tok.Line == position.Line+1 {
			start := tok.Column - 1
			end := start + len(tok.Literal)
			if position.Character >= start && position.Character <= end {
				return &tok
			}
		}
	}
	return nil
}

func lexAllTokens(l *lexer.Lexer) []lexer.Token {
	tokens := []lexer.Token{}
	for {
		tok := l.NextToken()
		tokens = append(tokens, tok)
		if tok.Type == lexer.TokenEOF {
			break
		}
	}
	return tokens
}

func (s *Server) handleDefinition(ctx context.Context, req *jsonrpc2.Request) (interface{}, error) {
	var params DefinitionParams
	if err := json.Unmarshal(*req.Params, &params); err != nil {
		return nil, err
	}

	doc := s.documents[params.TextDocument.URI]
	if doc == nil {
		return []Location{}, nil
	}

	position := params.Position

	// Find the token at the position
	token := findTokenAtPosition(doc.Lexer, position)
	if token == nil {
		return []Location{}, nil
	}

	// Retrieve symbol information
	symbol, exists := doc.Symbols[token.Literal]
	if !exists {
		return []Location{}, nil
	}

	location := Location{
		URI: params.TextDocument.URI,
		Range: Range{
			Start: Position{Line: symbol.Token.Line - 1, Character: symbol.Token.Column - 1},
			End:   Position{Line: symbol.Token.Line - 1, Character: symbol.Token.Column - 1 + len(symbol.Value)},
		},
	}

	return []Location{location}, nil
}

func findSymbolAtPosition(ast *parser.Program, position Position) *parser.Identifier {
	var foundSymbol *parser.Identifier
	parser.Inspect(ast, func(node parser.Node) bool {
		switch n := node.(type) {
		case *parser.Identifier:
			if n.Token.Line == position.Line+1 && n.Token.Column <= position.Character+1 && n.Token.Column+len(n.Value) >= position.Character+1 {
				foundSymbol = n
				return false // Stop traversal
			}
		}
		return true
	})
	return foundSymbol
}

func findDefinitionInAST(ast *parser.Program, name string) *parser.Identifier {
	var definition *parser.Identifier
	parser.Inspect(ast, func(node parser.Node) bool {
		switch n := node.(type) {
		case *parser.FunctionLiteral:
			if n.Name.Value == name {
				definition = n.Name
				return false
			}
		}
		return true
	})
	return definition
}

func (s *Server) handleCompletion(ctx context.Context, req *jsonrpc2.Request) (interface{}, error) {
	var params CompletionParams
	if err := json.Unmarshal(*req.Params, &params); err != nil {
		return nil, err
	}

	doc := s.documents[params.TextDocument.URI]
	if doc == nil {
		return []CompletionItem{}, nil
	}

	// Find the token at the current position
	token := findTokenAtPosition(doc.Lexer, params.Position)
	if token == nil {
		return []CompletionItem{}, nil
	}

	// Collect identifiers from the symbol table
	identifiers := collectIdentifiers(doc.Symbols)

	items := []CompletionItem{}
	for name := range identifiers {
		item := CompletionItem{
			Label: name,
			Kind:  6, // Function or Variable
		}
		items = append(items, item)
	}

	// Add keywords
	keywords := []string{"def", "return", "if", "else", "while", "for", "import", "defer", "go", "True", "False", "None"}
	for _, kw := range keywords {
		item := CompletionItem{
			Label: kw,
			Kind:  14, // Keyword
		}
		items = append(items, item)
	}

	return items, nil
}

func collectIdentifiers(symbols map[string]*parser.Identifier) map[string]struct{} {
	identifiers := make(map[string]struct{})
	for name := range symbols {
		identifiers[name] = struct{}{}
	}
	return identifiers
}

func main() {
	log.Println("Simple Language Server started.")
	server := &Server{
		documents: make(map[string]*Document),
	}

	handler := jsonrpc2.HandlerWithError(server.handle)
	stdin := os.Stdin
	stream := jsonrpc2.NewBufferedStream(stdin, jsonrpc2.VSCodeObjectCodec{})
	conn := jsonrpc2.NewConn(context.Background(), stream, handler)
	server.conn = conn

	<-conn.DisconnectNotify()
	log.Println("Simple Language Server stopped.")
}
