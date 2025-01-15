import React from 'react';

const ControlFlow: React.FC = () => {
  return (
    <div className="container mx-auto p-6">
      <section className="text-gray-800">
        <h1 className="text-3xl font-bold mb-4">Control Flow</h1>

        {/* Blocks Section */}
        <h2 className="text-2xl font-semibold mb-2">Blocks</h2>
        <p className="text-lg leading-relaxed mb-4">
          Blocks in Ç are expressions that return a value:
        </p>
        <pre className="bg-gray-200 text-black p-4 rounded-lg overflow-x-auto mb-4">
          <code className="text-sm">
            {`{
    // expressions
    return value    // explicit return
}`}
          </code>
        </pre>

        {/* Conditional Statements Section */}
        <h2 className="text-2xl font-semibold mb-2">Conditional Statements</h2>
        <p className="text-lg leading-relaxed mb-4">
          All condition statements are considered to be true if the result is 1, false otherwise.
        </p>
        <pre className="bg-gray-200 text-black p-4 rounded-lg overflow-x-auto mb-4">
          <code className="text-sm">
            {`// If expressions
if condition expression else expression
if condition expression
if(condition) expression

// Ternary syntax
u8 a = if condition 3 else 4

// the if and else branch MUST return the same type. so
if 1
    u8 a = 3
else
    u16 b = 5

// is NOT allowed. however, since blocks return void unless a return statement is used, this is allowed:

if 1 {
    u8 a = 3
} else {
    u16 b = 5
}`}
          </code>
        </pre>

        {/* Loops Section */}
        <h2 className="text-2xl font-semibold mb-2">Loops</h2>
        <p className="text-lg leading-relaxed mb-4">
          Here are examples of different loop structures in Ç:
        </p>
        <pre className="bg-gray-200 text-black p-4 rounded-lg overflow-x-auto mb-4">
          <code className="text-sm">
            {`// While loop
while condition expression // returns 1 (u8) if it has been ran at least once, else 0 (u8)

// For loop
for (expression; expression; expression) expression 
// the first expression is optional and is executed once before any loop iteration.
// the second expression is tested before every loop iteration. if it's equal to 1, the loop continues, otherwise it stops
// the third expression is optional and is ran after every loop iteration.
// returns 1 (u8) if it has been ran at least once, else 0 (u8)`}
          </code>
        </pre>
      </section>
    </div>
  );
};

export default ControlFlow;
