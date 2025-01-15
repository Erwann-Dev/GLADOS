import React from "react";

const Introduction: React.FC = () => {
  return (
    <div className="container mx-auto p-6">
      <section className="text-gray-800">
        <h1 className="text-3xl font-bold mb-4">Introduction</h1>
        <p className="text-lg leading-relaxed">
          The <span className="font-semibold">Ç programming language</span> is designed with a focus on safety and developer experience. 
          It combines modern programming features with low-level control, making it suitable for system programming while maintaining a clean and expressive syntax.
        </p>
      </section>
    </div>
  );
};

export default Introduction;
