import React from 'react';

const Functions: React.FC = () => {
  return (
    <div className="container mx-auto p-6">
      <section className="text-gray-800">
        <h1 className="text-3xl font-bold mb-4">Functions</h1>

        <p className="text-lg leading-relaxed mb-4">
          Functions in Ç are expressions that take parameters and return a value:
        </p>

        {/* Function Definition Section */}
        <h2 className="text-2xl font-semibold mb-2">Function Definition</h2>
        <pre className="bg-gray-200 text-black p-4 rounded-lg overflow-x-auto mb-4">
          <code className="text-sm">
            {`// Function definition
u8 addu8(u8 a, u8 b) a + b

u32 addu32(u32 a, u32 b) {
    return a + b
}`}
          </code>
        </pre>

        {/* Function Calls Section */}
        <h2 className="text-2xl font-semibold mb-2">Function Calls</h2>
        <pre className="bg-gray-200 text-black p-4 rounded-lg overflow-x-auto mb-4">
          <code className="text-sm">
            {`// Function calls
u8 result = addu8(3, 4)
u32 result2 = addu32(3, 4)`}
          </code>
        </pre>
      </section>
    </div>
  );
};

export default Functions;
