
type category = { id: int, label: string };
  
type item = { 
    id : int,
    title : string,
    categoryId : int,
    imageUrl : string,
    linkUrl : string,
    description : string,
    overlayColor : string
};

type portfolio = {
    catergories: list(category),
    items: list(item)
};
  
type model = {
    errorMessage: string,
    portfolio: portfolio,
    selectedCategoryId: int,
    selectedItemId: int,
    apiUrl: string
};
