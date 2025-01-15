import React from 'react';
import Code from '../components/Code';

const TypeCasting: React.FC = () => {
  return (
    <div className="container mx-auto p-6">
      <section className="text-gray-800">
        <h1 className="text-3xl font-bold mb-4">Type Casting</h1>
        <p className="text-lg leading-relaxed mb-4">
          Ç supports explicit type casting between compatible types:
        </p>

        <pre className="bg-gray-200 text-black p-4 rounded-lg overflow-x-auto mb-4">
          <code className="text-sm">
            {`\
// Numeric type casting
u8 a = 3
u32 b = a as u32

// Allowed casting operations:
// - Between enums and u64
// - Between ptr? and u64
// - Between ptr and u64
// - Between all numeric types\
            `}
          </code>
        </pre>

        <h2 className="text-2xl font-semibold mb-2">Allowed Casting Operations</h2>
        <ul className="list-disc pl-6 space-y-2 mb-4">
          <li className="text-lg">Between <Code>enums</Code> and <Code>u64</Code></li>
          <li className="text-lg">Between <Code>ptr?</Code> and <Code>u64</Code></li>
          <li className="text-lg">Between <Code>ptr</Code> and <Code>u64</Code></li>
          <li className="text-lg">Between all numeric types</li>
        </ul>
      </section>
    </div>
  );
};

export default TypeCasting;
