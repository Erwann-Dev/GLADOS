import React, { ReactNode } from "react";

const Code: React.FC<{children: ReactNode}> = ({ children }) => {
  return <code className='text-violet-500 bg-gray-200 px-1 py-0.5 rounded-sm'> { children }</code>
}

export default Code;
