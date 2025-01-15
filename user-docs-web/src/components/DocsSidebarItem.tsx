import { ChevronDown } from "lucide-react";
import { useState } from "react";

interface DocsItem {
  title: string;
}

export interface Article extends DocsItem {
  content: React.ReactNode
  articles?: never;
}

export interface Section extends DocsItem {
  articles: Article[];
  content?: never;
}

const DocsSidebarItem: React.FC<{
  item: Article | Section;
  depth: number;
  selectedTitle: string;
  onSelectedArticle: (title: string) => void;
}> = ({
  item,
  depth,
  selectedTitle,
  onSelectedArticle
}) => {
  const [showChildren, setShowChildren] = useState<boolean>(false);

  if (!!item.content) {
    return (
      <button
        onClick={() => onSelectedArticle?.(item.title)}
        className="group text-start px-2 py-1"
      >
        <span
          className={`group-hover:underline overflow-hidden text-nowrap ${
            selectedTitle === item.title
              ? "text-violet-600 font-bold"
              : "text-violet-900"
          }`}
        >
          {item.title}
        </span>
      </button>
    );
  }

  return (
    <div>
      <button
        onClick={() => setShowChildren(!showChildren)}
        className="text-start flex flex-row items-center justify-between rounded-md hover:bg-violet-200 text-violet-900 px-2 py-1 transition-all duration-150 w-full"
      >
        <span className="overflow-hidden text-nowrap">{item.title}</span>
        <div
          className={`ml-1 ${
            showChildren ? "rotate-0" : "rotate-180"
          } transition-all duration-150`}
        >
          <ChevronDown size={24} />
        </div>
      </button>
      {!!item.articles && (
        <div
          className={`${showChildren ? "hidden" : "flex flex-col border-l border-violet-900"}`}
          style={{ marginLeft: (depth + 1) * 16 + "px" }}
        >
          {item.articles.map((item) => (
            <DocsSidebarItem
              key={item.title}
              item={item}
              depth={depth + 1}
              selectedTitle={selectedTitle}
              onSelectedArticle={onSelectedArticle}
            />
          ))}
        </div>
      )}
    </div>
  );
};

export default DocsSidebarItem;
