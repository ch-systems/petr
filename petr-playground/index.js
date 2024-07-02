import { greet, run_snippet } from './pkg';

/*
greet('World');

let result = run_snippet("function main() returns 'int 42");


greet(result.toString());
*/

import * as monaco from 'monaco-editor';
// or import * as monaco from 'monaco-editor/esm/vs/editor/editor.api';
// if shipping only a subset of the features & languages is desired

monaco.editor.create(document.getElementById('monaco-editor'), {
	value: 'console.log("Hello, world")',
	language: 'javascript'
});
