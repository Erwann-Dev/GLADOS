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
enum status_e {
    active,
    inactive,
    undefined
}

status_e example = status_e::active\
            `}
          </code>
        </pre>

        {/* Structs Section */}
        <h2 className="text-2xl font-semibold mb-2">Structs</h2>
        <pre className="bg-gray-200 text-black p-4 rounded-lg overflow-x-auto mb-4">
          <code className="text-sm">
            {`\
struct rectangle_s {
    u8 width,
    u8 height
}

mut rectangle_s example = rectangle_s {width: 3, height: 4}
u8 value = example.width\
            `}
          </code>
        </pre>
      </section>
    </div>
  );
};

export default UserDefinedTypes;
