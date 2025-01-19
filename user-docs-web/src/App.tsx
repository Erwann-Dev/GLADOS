import React from 'react'
import cLogo from '/c_logo.svg'
import { DocsMobileSidebar, DocsSidebar } from './components/DocsSidebar'

const App: React.FC = () => {
  return (
    <>
      <div className='hidden md:block'>
        <DocsSidebar />
      </div>
      <div className='block md:hidden'>
        <DocsMobileSidebar />
      </div>
    </>
  )
}

export default App
