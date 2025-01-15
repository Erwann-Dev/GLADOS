import React from 'react';

const Variables: React.FC = () => {
  return (
    <div className="container mx-auto p-6">
      <section className="text-gray-800">
        <h1 className="text-3xl font-bold mb-4">Variables</h1>

        <h2 className="text-2xl font-semibold mb-2">Declaration and Initialization</h2>
        <p className="text-lg leading-relaxed mb-4">
          Variables in Ç must be initialized at declaration. The language supports both mutable and immutable variables:
        </p>

        <pre className="bg-gray-200 text-black p-4 rounded-lg overflow-x-auto mb-4">
          <code className="text-sm">
            {`\
u8 a = 3          // immutable
mut u8 b = 3      // mutable
f32 c = 3.0       // immutable float
mut f64 d = 3.    // mutable double
mut f32 e = .051  // mutable float

// declarations and assignments return the new value of the variable, so this is allowed
u8 a = u8 b = 5\
            `}
          </code>
        </pre>

        <h2 className="text-2xl font-semibold mb-2">Rules</h2>
        <ul className="list-disc pl-6 space-y-2 mb-4">
          <li className="text-lg leading-relaxed">Variables must be initialized at declaration</li>
          <li className="text-lg leading-relaxed">Only mutable variables can be reassigned</li>
          <li className="text-lg leading-relaxed">
            Variable shadowing is not allowed on the same scope (block) but is allowed for nested blocks
          </li>
        </ul>
      </section>
    </div>
  );
};

export default Variables;
