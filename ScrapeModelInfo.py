# install chromedriver from https://chromedriver.storage.googleapis.com/index.html?path=2.38/
from selenium import webdriver

AutoURL = "https://www.autotrader.com/cars-for-sale/vehicledetails.xhtml?listingId=466254090&zip=84604&referrer=%2Fcars-for-sale%2Fsearchresults.xhtml%3Fzip%3D84604%26startYear%3D1981%26sortBy%3Drelevance%26incremental%3Dall%26firstRecord%3D350%26endYear%3D2019%26modelCodeList%3DWRANGLER%26makeCodeList%3DJEEP%26searchRadius%3D50&startYear=1981&numRecords=25&firstRecord=350&endYear=2019&modelCodeList=WRANGLER&makeCodeList=JEEP&searchRadius=50&makeCode1=JEEP&modelCode1=WRANGLER"

ID = "466254090-pane-2"
CSSSelectorButton = ".active+ li a , .active+ li span"

class AutoTraderSession():
    def __init__(self):
        self.browser = webdriver.Chrome(r'chromedriver')

    def getData(self, url, CSSselector, ID):
        self.browser.get(url)
        self.browser.find_element_by_css_selector(CSSselector).click()
        table_data = self.browser.find_elements_by_id(ID)
        list_rows = []
        #for items in table_data.find_elements_by_xpath('.//tr'):
        #    list_cells = []
        #    for item in items.find_elements_by_xpath('.//td[@class="row"]|.//td'):
        #        list_cells.append(item.text)
        #        list_rows.append(list_cells)
        #for data in list_rows:
        #    print(data)
        print(table_data)
        self.browser.quit()



def main():
    Session = AutoTraderSession()
    Session.getData(url = AutoURL, CSSselector = CSSSelectorButton, ID = ID)




if __name__ == '__main__':
    main()
