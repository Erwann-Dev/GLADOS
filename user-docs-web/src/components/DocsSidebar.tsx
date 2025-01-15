import { Menu, X } from "lucide-react";
import React, { useEffect, useState } from "react";
import cLogo from "/c_logo.svg";
import DocsSidebarItem, { Article, Section } from "./DocsSidebarItem";

const articles: (Article | Section)[] = [
  { title: "Introduction", content: <h1>tos</h1> },
  { title: "System Requirements", content: <h1>tos</h1> },
  {
    title: "Types",
    articles: [
      { title: "Basic types", content: <h1>how-does-it-work</h1> },
      { title: "Variables", content: <h1>how-does-it-work</h1> },
      { title: "Pointers", content: <h1>how-does-it-work</h1> },
      { title: "Strings", content: <h1>tos</h1> },
      { title: "Type casting", content: <h1>tos</h1> },
      { title: "User defined types", content: <h1>tos</h1> },
    ],
  },
  { title: "Control flow", content: <h1>tos</h1> },
  { title: "Functions", content: <h1>tos</h1> },
  { title: "Operators", content: <h1>tos</h1> },
  { title: "System Calls", content: <h1>tos</h1> },
  { title: "Preprocessors", content: <h1>tos</h1> },
  { title: "Future features", content: <h1>tos</h1> },
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
    <div className="flex flex-row p-5 min-h-dvh ">
      <div className="hidden sm:block border border-violet-900 p-1.5 rounded-2xl min-w-56">
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
      <div className="ml-5 flex-1">
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
