import React from 'react';

const Strings: React.FC = () => {
  return (
    <div className="container mx-auto p-6">
      <section className="text-gray-800">
        <h1 className="text-3xl font-bold mb-4">Strings</h1>
        <p className="text-lg leading-relaxed mb-4">
          Strings in Ç are automatically converted to null-terminated C-style arrays:
        </p>

        <pre className="bg-gray-200 text-black p-4 rounded-lg overflow-x-auto mb-4">
          <code className="text-sm">
            {`\
"Hello, World!"
// Becomes [72, 101, 108, 108, 111, 44, 32, 87, 111, 114, 108, 100, 33, 0]\
            `}
          </code>
        </pre>
      </section>
    </div>
  );
};

export default Strings;

