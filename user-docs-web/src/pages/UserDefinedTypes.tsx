import React from 'react';

const UserDefinedTypes: React.FC = () => {
  return (
    <div className="container mx-auto p-6">
      <section className="text-gray-800">
        <h1 className="text-3xl font-bold mb-4">User-Defined Types</h1>

        {/* Enums Section */}
        <h2 className="text-2xl font-semibold mb-2">Enums</h2>
        <pre className="bg-gray-200 text-black p-4 rounded-lg overflow-x-auto mb-4">
          <code className="text-sm">
            {`\
enum test { a, b, c }

test example = test.a\
            `}
          </code>
        </pre>

        {/* Structs Section */}
        <h2 className="text-2xl font-semibold mb-2">Structs</h2>
        <pre className="bg-gray-200 text-black p-4 rounded-lg overflow-x-auto mb-4">
          <code className="text-sm">
            {`\
struct test {
    u8 a,
    u8 b
}

mut test example = test {a: 3, b: 4}
u8 value = example.a\
            `}
          </code>
        </pre>
      </section>
    </div>
  );
};

export default UserDefinedTypes;
