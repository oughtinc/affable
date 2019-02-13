import globals from 'rollup-plugin-node-globals'; // axios shenanigans
import builtins from 'rollup-plugin-node-builtins'; // axios shenanigans
import json from 'rollup-plugin-json'; // axios shenanigans
import resolve from 'rollup-plugin-node-resolve';
import commonjs from 'rollup-plugin-commonjs';
import typescript from 'rollup-plugin-typescript2';

export default {
    input: 'ts/workspace.ts',
    output: {
        file: 'static/bundle.js',
        format: 'iife',
        name: 'affable'
    },
    plugins: [
        resolve({jsnext: true, preferBuiltins: true, browser: true}),
        commonjs({include: 'node_modules/**'}),
        globals(),
        builtins(),
        json(),
        typescript()
    ]
};
