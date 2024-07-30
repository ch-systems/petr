import { run_snippet, format } from './pkg';


import * as monaco from 'monaco-editor';
// or import * as monaco from 'monaco-editor/esm/vs/editor/editor.api';
// if shipping only a subset of the features & languages is desired

// config for petr as a custom language
// // Register a new language
monaco.languages.register({ id: "petr" });

// Register a tokens provider for the language
monaco.languages.setMonarchTokensProvider("petr", {
  keywords: [ 'fn', 'returns', 'in', 'type', 'export' ],
	tokenizer: {
		root: [
      [/\~([a-zA-Z_][a-zA-Z0-9_]+)(\.[a-zA-Z_]([a-zA-Z0-9_])+)*/, "function-call"],
			[/\@[a-zA-Z_]+/, "intrinsic"],
			[/[0-9]+/, "integer-literal"],
      [/\".*\"/, "string-literal"],
      [/\{-.*-\}/, "comment"],
		],
	},
});

monaco.languages.setLanguageConfiguration("petr", {
  	brackets: [
		['[', ']'],
		['(', ')']
	],
	autoClosingPairs: [
		{ open: '[', close: ']' },
		{ open: '(', close: ')' },
		{ open: '"', close: '"' }
	],
	surroundingPairs: [
		{ open: '{', close: '}' },
		{ open: '[', close: ']' },
		{ open: '(', close: ')' },
		{ open: '"', close: '"' }
	]
})

const literalColor = "ff0000";

// Define a new theme that contains only rules that match this language
monaco.editor.defineTheme("petr-theme", {
	base: "vs-dark",
	inherit: true,
	rules: [
		{ token: "intrinsic", foreground: "808080", fontStyle: "bold" },
		{ token: "function-call", foreground: "808080", fontStyle: "bold" },
		{ token: "string-literal", foreground: literalColor },
		{ token: "integer-literal", foreground: literalColor },
		{ token: "keyword", foreground: literalColor },
		{ token: "comment", foreground: "C4A484", fontStyle: "italic"},
	],
	colors: {
		"editor.foreground": "#ffffff",
	},
});

/*
// Register a completion item provider for the new language
monaco.languages.registerCompletionItemProvider("mySpecialLanguage", {
	provideCompletionItems: (model, position) => {
		var word = model.getWordUntilPosition(position);
		var range = {
			startLineNumber: position.lineNumber,
			endLineNumber: position.lineNumber,
			startColumn: word.startColumn,
			endColumn: word.endColumn,
		};
		var suggestions = [
			{
				label: "simpleText",
				kind: monaco.languages.CompletionItemKind.Text,
				insertText: "simpleText",
				range: range,
			},
			{
				label: "testing",
				kind: monaco.languages.CompletionItemKind.Keyword,
				insertText: "testing(${1:condition})",
				insertTextRules:
					monaco.languages.CompletionItemInsertTextRule
						.InsertAsSnippet,
				range: range,
			},
			{
				label: "ifelse",
				kind: monaco.languages.CompletionItemKind.Snippet,
				insertText: [
					"if (${1:condition}) {",
					"\t$0",
					"} else {",
					"\t",
					"}",
				].join("\n"),
				insertTextRules:
					monaco.languages.CompletionItemInsertTextRule
						.InsertAsSnippet,
				documentation: "If-Else Statement",
				range: range,
			},
		];
		return { suggestions: suggestions };
	},
});
*/




monaco.editor.create(document.getElementById('monaco-editor'), {
	value: "fn main() returns 'unit \n  ~std.io.print \"Hello, World!\"",
	language: 'petr',
  theme: "petr-theme",
});

export function setOutputContent(content) {
  document.getElementById('output').innerHTML = content;
}
window.setOutputContent = setOutputContent;

export function setCodeEditorContent(content) {
  monaco.editor.getModels()[0].setValue(content);
}

window.setCodeEditorContent= setCodeEditorContent;

// set on-clicks for the buttons
document.getElementById('run').onclick = function() {
  // get the text content from the monaco instance
  let code = monaco.editor.getModels()[0].getValue();
  let result = "";
  // run the code
  // TODO: actually render the errors on the span in the diagnostics of monaco
   try { result = run_snippet(code); } catch (e) { 
    // set the output to the diagnostics
    // because an Err result from wasm becomes an exception
    // might be good to not use Result for that reason
    result.replace("\n", "<br>");
    document.getElementById('output').innerHTML = e;
    return;
  };
}
document.getElementById('format').onclick = function() {
  // get the text content from the monaco instance
  let code = monaco.editor.getModels()[0].getValue();
  // run the code
  // TODO: actually render the errors on the span in the diagnostics of monaco
   try { format(code); } catch (e) { 
    // set the output to the diagnostics
    // because an Err result from wasm becomes an exception
    // might be good to not use Result for that reason
    document.getElementById('output').innerHTML = e;
    return;
  };
}
