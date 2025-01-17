import React from 'react';
import Code from '../components/Code';

const BasicTypes: React.FC = () => {
  const integerTypes = [
    { type: 'u8', description: '8-bit unsigned integer' },
    { type: 'u16', description: '16-bit unsigned integer' },
    { type: 'u32', description: '32-bit unsigned integer' },
    { type: 'u64', description: '64-bit unsigned integer' },
    { type: 'i8', description: '8-bit signed integer' },
    { type: 'i16', description: '16-bit signed integer' },
    { type: 'i32', description: '32-bit signed integer' },
    { type: 'i64', description: '64-bit signed integer' },
  ];

  const floatingPointTypes = [
    { type: 'f32', description: '32-bit floating-point' },
    { type: 'f64', description: '64-bit floating-point' },
  ];

  const otherTypes = [
    { type: 'void', description: 'represents the absence of a type' },
  ];

  return (
    <div className="container mx-auto p-6">
      <section className="text-gray-800">
        <h1 className="text-3xl font-bold mb-4">Basic Types</h1>
        <p className="text-lg leading-relaxed mb-4">
          Ç provides a comprehensive set of primitive types:
        </p>

        <h2 className="text-2xl font-semibold mb-2">Integer Types</h2>
        <ul className="list-disc pl-6 space-y-2 mb-4">
          {integerTypes.map((item, index) => (
            <li key={index} className="text-lg">
              <Code>{item.type}</Code>: {item.description}
            </li>
          ))}
        </ul>

        <h2 className="text-2xl font-semibold mb-2">Floating-Point Types</h2>
        <ul className="list-disc pl-6 space-y-2 mb-4">
          {floatingPointTypes.map((item, index) => (
            <li key={index} className="text-lg">
              <Code>{item.type}</Code>: {item.description}
            </li>
          ))}
        </ul>

        <h2 className="text-2xl font-semibold mb-2">Other Types</h2>
        <ul className="list-disc pl-6 space-y-2 mb-4">
          {otherTypes.map((item, index) => (
            <li key={index} className="text-lg">
              <Code>{item.type}</Code>: {item.description}
            </li>
          ))}
          <li className='text-lg'>
            Boolean values are typically implemented using <Code>u8</Code> with preprocessor directive:<br />
            <Code>#define bool u8</Code><br/>
            <Code>#define TRUE 1</Code><br />
            <Code>#define FALSE 0</Code>
          </li>
        </ul>
      </section>
    </div>
  );
};

export default BasicTypes;
