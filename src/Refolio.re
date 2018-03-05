module Option = {
  let unwrapUnsafely = data =>
    switch data {
    | Some(v) => v
    | None => raise(Invalid_argument("unwrapUnsafely called on None"))
    };
};

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

module Decode = {
  let category = json =>
    Json.Decode.{
      id: json |> field("id", int),
      label: json |> field("label", string)
    };
  let item = json =>
    Json.Decode.{
      id: json |> field("id", int),
      title: json |> field("title", string),
      categoryId: json |> field("categoryId", int),
      imageUrl: json |> field("imageUrl", string),
      linkUrl: json |> field("linkUrl", string),
      description: json |> field("description", string),
      overlayColor: json |> field("overlayColor", string)
    };
  let portfolio = json =>
    Json.Decode.{
      categories: json |> Json.Decode.list(category),
      items: json |> Json.Decode.list(item)
    };
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
  didMount: ({state: {apiUrl}, reduce}) => {
    Js.Promise.(
      Fetch.fetch(apiUrl)
      |> then_(Fetch.Response.text)
      |> then_(text => print_endline(text) |> resolve)
    );
    ReasonReact.NoUpdate;
  },
  reducer: (action, reduce) =>
    switch action {
    | ApiResponse(string) => ReasonReact.NoUpdate
    | CategoryClicked(id) => ReasonReact.NoUpdate
    | ItemClicked(id) => ReasonReact.NoUpdate
    },
  render: ({state: {apiUrl}, reduce}) =>
    <div className="app">
      <h1 className="title"> (str("Refolio")) </h1>
      <div> (str(apiUrl)) </div>
      <div className="footer" />
    </div>
};
