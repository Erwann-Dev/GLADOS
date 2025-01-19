import React from 'react';
import Code from '../components/Code';

const Operators: React.FC = () => {
  return (
    <div className="container mx-auto p-6">
      <section className="text-gray-800">
        <h1 className="text-3xl font-bold mb-4">Operators</h1>

        {/* Comparison Operators Section */}
        <h2 className="text-2xl font-semibold mb-2">Comparison Operators</h2>
        <p className="text-lg leading-relaxed mb-4">
          All comparison operators return <Code>u8</Code> (1 for true, 0 for false):
        </p>
        <ul className="list-disc pl-6 space-y-2 mb-4">
          <li className="text-lg"> <Code>&#62;</Code>, <Code>&#60;</Code>, <Code>&#62;=</Code>, <Code>&#60;=</Code></li>
          <li className="text-lg"><Code>==</Code>, <Code>!=</Code></li>
          <li className="text-lg"><Code>and</Code>, <Code>or</Code></li>
        </ul>

        {/* Not Operator Section */}
        <h2 className="text-2xl font-semibold mb-2">Not Operator</h2>
        <p className="text-lg leading-relaxed mb-4">
          The not operator is only valid on <Code>u8</Code>. It returns 1 (u8) if the expression is not equal to 1, otherwise it returns 0 (u8).
        </p>
        <pre className="bg-gray-200 text-black p-4 rounded-lg overflow-x-auto mb-4">
          <code className="text-sm">
            {`u8 a = 0
u8 b = not a // this is equal to 1`}
          </code>
        </pre>

        {/* Arithmetic Operators Section */}
        <h2 className="text-2xl font-semibold mb-2">Arithmetic Operators</h2>
        <p className="text-lg leading-relaxed mb-4">
          Arithmetic operators:
        </p>
        <ul className="list-disc pl-6 space-y-2 mb-4">
          <li className="text-lg"><Code>+</Code>, <Code>-</Code>, <Code>+=</Code>, <Code>-=</Code>: Addition and subtraction</li>
          <li className="text-lg"><Code>*</Code>, <Code>/</Code>, <Code>%</Code>, <Code>*=</Code>, <Code>/=</Code>, <Code>%=</Code>: Multiplication, division, and modulo</li>
        </ul>
        <p className="text-lg leading-relaxed mb-4">
          Note: All arithmetic operators require operands of the same type and include runtime overflow/underflow checking. An underflow, overflow, or math error will result in a runtime error.
        </p>

        {/* sizeof Operator Section */}
        <h2 className="text-2xl font-semibold mb-2">sizeof Operator</h2>
        <p className="text-lg leading-relaxed mb-4">
          The <Code>sizeof</Code> operator returns the size in bytes of a type or expression at compile time. It can be used with any type or variable and returns a <Code>u64</Code>.
        </p>
        <pre className="bg-gray-200 text-black p-4 rounded-lg overflow-x-auto mb-4">
          <code className="text-sm">
            {`// Basic types
u64 size_int = sizeof i32        // 4
u64 size_long = sizeof u64       // 8
u64 size_float = sizeof f32      // 4
// Arrays
ptr u8 arr = [1, 2, 3, 4]
u64 arr_size = sizeof arr        // size of pointer (8)
u64 elem_size = sizeof arr[0]    // size of element (1)
// Structs
struct Point {
  i32 x,
  i32 y
}
u64 struct_size = sizeof Point   // 8 (4 + 4)
// Expressions
u64 expr_size = sizeof(3 + 4)    // size of result type`}
          </code>
        </pre>
      </section>
    </div>
  );
};

export default Operators;
