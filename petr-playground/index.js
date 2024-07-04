import {  run_snippet } from './pkg';


import * as monaco from 'monaco-editor';
// or import * as monaco from 'monaco-editor/esm/vs/editor/editor.api';
// if shipping only a subset of the features & languages is desired
//

// config for petr as a custom language
// // Register a new language
monaco.languages.register({ id: "petr" });

// Register a tokens provider for the language
monaco.languages.setMonarchTokensProvider("petr", {
  keywords: [ 'function', 'returns', 'in' ],
	tokenizer: {
		root: [
      [/\~[a-zA-Z][a-zA-Z0-9]*/, "function-call"],
			[/\@[a-zA-Z]+/, "intrinsic"],
			[/[0-9]+/, "integer-literal"],
      [/\".*\"/, "string-literal"],
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
	value: "function main() returns 'unit \n  @puts(\"Hello, World!\")",
	language: 'petr',
  theme: "petr-theme",
});

export function setOutputContent(content) {
  document.getElementById('output').innerHTML = content;
}
window.setOutputContent = setOutputContent;

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
