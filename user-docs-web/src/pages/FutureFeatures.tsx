import React from 'react';

const FutureFeatures: React.FC = () => {
  return (
    <div className="container mx-auto p-6">
      <section className="text-gray-800">
        <h1 className="text-3xl font-bold mb-4">Future Features</h1>

        <p className="text-lg leading-relaxed mb-4">
          The following features are planned for future releases:
        </p>

        <ul className="list-disc pl-6 space-y-2 mb-4">
          <li className="text-lg">Standard library</li>
          <li className="text-lg">Pattern matching</li>
          <li className="text-lg">Function overloading</li>
          <li className="text-lg">Higher order functions</li>
          <li className="text-lg">Enhanced struct capabilities</li>
          <li className="text-lg">Algebraic enums</li>
          <li className="text-lg">Traits and implementations</li>
          <li className="text-lg">Reference-counted heap allocation</li>
          <li className="text-lg">Bitwise operators</li>
        </ul>
      </section>
    </div>
  );
};

export default FutureFeatures;
