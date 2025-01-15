import React from 'react';
import Code from '../components/Code';

const Preprocessor: React.FC = () => {
  return (
    <div className="container mx-auto p-6">
      <section className="text-gray-800">
        <h1 className="text-3xl font-bold mb-4">Preprocessor</h1>

        {/* Comments Section */}
        <h2 className="text-2xl font-semibold mb-2">Comments</h2>
        <p className="text-lg leading-relaxed mb-4">
          Ç supports both single-line and multi-line comments. Here's an example:
        </p>
        <pre className="bg-gray-200 text-black p-4 rounded-lg overflow-x-auto mb-4">
          <code className="text-sm">
            {`\
// Single-line comment

/*
   Multi-line
   comment
   are
   supported
*/

// however, this style
// of multi-line comments
// is recommended instead\
            `}
          </code>
        </pre>

        {/* Directives Section */}
        <h2 className="text-2xl font-semibold mb-2">Directives</h2>
        <p className="text-lg leading-relaxed mb-4">
          Preprocessor directives in Ç include macros and conditional compilation:
        </p>
      <pre className="bg-gray-200 text-black p-4 rounded-lg overflow-x-auto mb-4">
          <code className="text-sm">
            {`#define CONSTANT
#define CONSTANT value
#undef CONSTANT
#ifdef CONSTANT
#ifndef CONSTANT
#endif`}
          </code>
        </pre>

        {/* Include Section */}
        <h2 className="text-2xl font-semibold mb-2">Include</h2>
        <p className="text-lg leading-relaxed mb-4">
          The <Code>#include</Code> directive is used to include external files in the code:
        </p>
        <pre className="bg-gray-200 text-black p-4 rounded-lg overflow-x-auto mb-4">
          <code className="text-sm">
            {`#include "../include/file.cc"  // relative path
#include "file.cc"            // local path`}
          </code>
        </pre>
      </section>
    </div>
  );
};

export default Preprocessor;
