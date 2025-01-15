import React from 'react';

const SystemRequirements: React.FC = () => {
  const requirements = [
    "64-bit little-endian Linux systems",
    "GNU C Library (glibc) is not required",
    "May work on other systems (not guaranteed)",
  ];

  return (
    <div className="container mx-auto p-6">
      <section className="text-gray-800">
        <h1 className="text-3xl font-bold mb-4">System Requirements</h1>
        <ul className="list-disc pl-6 space-y-2">
          {requirements.map((requirement, index) => (
            <li key={index} className="text-lg leading-relaxed">
              {requirement}
            </li>
          ))}
        </ul>
      </section>
    </div>
  );
};

export default SystemRequirements;
