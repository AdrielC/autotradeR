# install chromedriver from https://chromedriver.storage.googleapis.com/index.html?path=2.38/
from selenium import webdriver

AutoURL = "https://www.autotrader.com/cars-for-sale/vehicledetails.xhtml?listingId=479576919&zip=35801&referrer=%2Fcars-for-sale%2Fsearchresults.xhtml%3Fzip%3D35801%26startYear%3D1981%26numRecords%3D100%26sortBy%3Drelevance%26incremental%3Dall%26firstRecord%3D900%26endYear%3D2019%26modelCodeList%3DWRANGLER%26makeCodeList%3DJEEP%26searchRadius%3D0&startYear=1981&numRecords=100&firstRecord=900&endYear=2019&modelCodeList=WRANGLER&makeCodeList=JEEP&searchRadius=0&makeCode1=JEEP&modelCode1=WRANGLER&digitalRetail=true"

ID = "466254090-pane-2"
CSSSelectorButton = ".active+ li a , .active+ li span"

class AutoTraderSession():
    def __init__(self):
        self.browser = webdriver.Chrome(r'chromedriver')

    def getData(self, url, CSSselector):
        self.browser.get(url)
        self.browser.find_element_by_css_selector(CSSselector).click()
        return self.browser.page_source

def main():
    Session = AutoTraderSession()
    return Session.getData(url = AutoURL, CSSselector = CSSSelectorButton)
    Session.browser.quit()

if __name__ == '__main__':
    main()
