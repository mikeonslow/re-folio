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
  catergories: list(category),
  items: list(item)
};

type model = {
  errorMessage: string,
  portfolio,
  selectedCategoryId: option(int),
  selectedItemId: option(int),
  apiUrl: string
};

type action =
  | ApiResponse(string) /* `string` will become the API response handler */
  | CategoryClicked(int)
  | ItemClicked(int);
