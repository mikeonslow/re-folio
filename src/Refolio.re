type category = {
  id: int,
  label: string
};

type item = {
  id: int,
  title: string,
  categoryId: int,
  imageUrl: string,
  linkUrl: string,
  description: string,
  overlayColor: string
};

type portfolio = {
  categories: list(category),
  items: list(item)
};

type state = {
  errorMessage: option(string),
  portfolio,
  selectedCategoryId: option(int),
  selectedItemId: option(int),
  apiUrl: string
};

type action =
  | ApiResponse(string) /* `string` will become the API response handler */
  | CategoryClicked(int)
  | ItemClicked(int);

let str = ReasonReact.stringToElement;

let component = ReasonReact.reducerComponent("Refolio");

let make = children => {
  ...component,
  initialState: () => {
    errorMessage: None,
    portfolio: {
      categories: [],
      items: []
    },
    selectedCategoryId: None,
    selectedItemId: None,
    apiUrl: "http://www.mocky.io/v2/59f8cfa92d0000891dad41ed"
  },
  reducer: (action, reduce) =>
    switch action {
    | ApiResponse(string) => ReasonReact.NoUpdate
    | CategoryClicked(id) => ReasonReact.NoUpdate
    | ItemClicked(id) => ReasonReact.NoUpdate
    },
  render: ({state: {apiUrl}, reduce}) =>
    <div className="app">
      <div className="title" />
      <div> (str(apiUrl)) </div>
      <div className="footer" />
    </div>
};
