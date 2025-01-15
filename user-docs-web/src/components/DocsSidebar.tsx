import { FunctionSquareIcon, Menu, X } from "lucide-react";
import React, { useEffect, useState } from "react";
import cLogo from "/c_logo.svg";
import DocsSidebarItem, { Article, Section } from "./DocsSidebarItem";
import Introduction from "../pages/Introduction"
import SystemRequirements from "../pages/SystemRequirements";
import BasicTypes from "../pages/BasicTypes";
import Variables from "../pages/Variables";
import Pointers from "../pages/Pointers";
import Strings from "../pages/Strings";
import TypeCasting from "../pages/TypeCasting";
import UserDefinedTypes from "../pages/UserDefinedTypes";
import ControlFlow from "../pages/ControlFlow";
import Functions from "../pages/Functions";
import Operators from "../pages/Operators";
import SystemCalls from "../pages/SystemCalls";
import Preprocessor from "../pages/Preprocessor";
import FutureFeatures from "../pages/FutureFeatures";

const articles: (Article | Section)[] = [
  { title: "Introduction", content: <Introduction />},
  { title: "System Requirements", content: <SystemRequirements /> },
  {
    title: "Types",
    articles: [
      { title: "Basic types", content: <BasicTypes /> },
      { title: "Variables", content: <Variables /> },
      { title: "Pointers", content: <Pointers /> },
      { title: "Strings", content: <Strings /> },
      { title: "Type casting", content: <TypeCasting /> },
      { title: "User defined types", content: < UserDefinedTypes /> },
    ],
  },
  { title: "Control flow", content: <ControlFlow /> },
  { title: "Functions", content: <Functions /> },
  { title: "Operators", content: <Operators /> },
  { title: "System Calls", content: <SystemCalls /> },
  { title: "Preprocessors", content: <Preprocessor /> },
  { title: "Future features", content: <FutureFeatures /> },
];

const findRoute: (route: string, items: (Article | Section)[]) => Article | undefined = (
  route,
  items
) => {
  for (const item of items) {

    if (!!item.content) { // Article
      if (item.title === route)
        return item;
    }

    else if (!!item.articles) { // Section
      const found = findRoute(route, item.articles);
      if (found)
        return found;
    }
  }
  return undefined;
}

const DocsSidebar: React.FC = () => {
  const [ location, setLocation ] = useState<string>("How does it work");
  const [ content, setContent ] = useState<React.ReactNode>(<h1>default</h1>);

  useEffect(() => {
    const found = findRoute(location, articles);
    if (found)
      setContent(found.content);
  }, [location]);

  return (
    <div className="flex flex-row h-dvh p-5">
      <div className="border border-violet-900 p-1.5 rounded-2xl min-w-56">
        <div className="border-2 border-violet-900 flex flex-col bg-violet-50 rounded-xl px-2 sm:px-3 py-2 h-full">
          <div className="flex flex-row items-center mb-2">
            <img src={cLogo} className="h-8" alt="Logo" />
            <h1 className="ml-3 font-bold text-2xl text-violet-900"> Docs </h1>
          </div>
          {articles.map((item) => (
            <DocsSidebarItem
              key={item.title}
              item={item}
              depth={0}
              selectedTitle={location}
              onSelectedArticle={setLocation}
            />
          ))}
        </div>
      </div>
      <div className="ml-5 flex-1 overflow-auto">
        { content }
      </div>
    </div>
  );
};

const DocsMobileSidebar: React.FC = () => {
  const [isMenuOpen, setIsMenuOpen] = useState(false);

  const [ location, setLocation ] = useState<string>("How does it work");
  const [ content, setContent ] = useState<React.ReactNode>(<h1>default</h1>);

  useEffect(() => {
    const found = findRoute(location, articles);
    if (found)
      setContent(found.content);
  }, [location]);

  const handleNavigation = (title: string) => {
    setLocation(title);
    setIsMenuOpen(false);
  };

  return (
    <>
      <div
        className={`
          fixed left-0 top-0 h-dvh w-screen z-50
          ${isMenuOpen ? "max-w-[80%]" : "max-w-0"}
          transition-all duration-300 h-full overflow-hidden
        `}
      >
        <div className="border-2 border-violet-900 flex flex-col bg-violet-50 h-full">
          <div className="flex flex-row items-center p-4 border-b-2 border-violet-900">
            <img src={cLogo} className="h-10" alt="Hypurr logo" />
            <h1 className="ml-3 font-bold text-2xl text-violet-900"> Docs </h1>
            <button
              onClick={() => setIsMenuOpen(false)}
              className="text-violet-900 ml-auto"
            >
              <X size={32} />
            </button>
          </div>

          <div className="flex-1 overflow-y-auto p-4">
            {articles.map((item) => (
              <DocsSidebarItem
                key={item.title}
                item={item}
                depth={0}
                selectedTitle={location}
                onSelectedArticle={handleNavigation}
              />
            ))}
          </div>
        </div>
      </div>

      <div className="m-4" >
        <div className="flex flex-row items-center mb-4">
          <button
            onClick={() => setIsMenuOpen(true)}
            className="text-violet-900 p-1.5 border border-violet-900 rounded-md bg-white"
            aria-label="Toggle menu"
          >
            <Menu size={32} />
          </button>
          <h1 className="ml-3 font-bold text-2xl text-violet-900"> Docs </h1>
        </div>
        { content }
      </div>
    </>
  );
};

export { DocsSidebar, DocsMobileSidebar };
