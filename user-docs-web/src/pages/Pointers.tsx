import React from 'react';
import Code from '../components/Code';

const Pointers: React.FC = () => {
  return (
    <div className="container mx-auto p-6">
      <section className="text-gray-800">
        <h1 className="text-3xl font-bold mb-4">Pointers</h1>
        <p className="text-lg leading-relaxed mb-4">
          Ç provides a safe and expressive pointer system that differs from traditional C pointers.
        </p>

        <h2 className="text-2xl font-semibold mb-2">Pointer Types</h2>
        <ul className="list-disc pl-6 space-y-2 mb-4">
          <li className="text-lg">
            <Code>ptr type</Code>: Non-null immutable pointer to an immutable type
          </li>
          <li className="text-lg">
            <Code>ptr mut type</Code>: Non-null immutable pointer to a mutable type
          </li>
          <li className="text-lg">
            <Code>ptr? type</Code>: Nullable mutable pointer to an immutable type
          </li>
          <li className="text-lg">
            <Code>ptr? ptr? mut type</Code>: Nullable mutable pointer to a nullable mutable pointer to a mutable type
          </li>
        </ul>

        <h2 className="text-2xl font-semibold mb-2">Pointer Operations</h2>
        <p className="text-lg leading-relaxed mb-4">
          Below are some examples of pointer operations:
        </p>

        <pre className="bg-gray-200 text-black p-4 rounded-lg overflow-x-auto mb-4">
          <code className="text-sm">
            {`// Reference operator
u8 a = 3
ptr u8 b = &a
ptr mut u8 b2 = &a // not allowed.

// Array initialization
ptr u8 c = [1, 2, 3, 4, 5]

// Dereferencing
*b      // dereference operator.
b[0]    // array syntax (equivalent to *b)
b[x]    // equivalent to *(b + x * sizeof b)

mut u8 d = 45
ptr u8 e = &d // allowed, removes the mutability of d`}
          </code>
        </pre>
      </section>
    </div>
  );
};

export default Pointers;
