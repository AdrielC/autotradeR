# install chromedriver from https://chromedriver.storage.googleapis.com/index.html?path=2.38/
# HANDY LINK: http://isaacviel.name/make-web-driver-wait-element-become-visiable/
import sys
import warnings
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC

URL = "https://www.autotrader.com/cars-for-sale/vehicledetails.xhtml?listingId=484656454&zip=84604&referrer=%2Fcars-for-sale%2Fsearchresults.xhtml%3Fzip%3D84604%26startYear%3D1981%26sortBy%3Drelevance%26firstRecord%3D0%26endYear%3D2019%26modelCodeList%3DWRANGLER%26makeCodeList%3DJEEP%26searchRadius%3D25&startYear=1981&numRecords=25&firstRecord=0&endYear=2019&modelCodeList=WRANGLER&makeCodeList=JEEP&searchRadius=25"

for path in sys.path:
    print(path)

print(sys.path[0])

class AutoTraderSession():
    def __init__(self):
        self.driver = webdriver.PhantomJS(executable_path=sys.path[0]) # PATH to installation of PhantomJS
        self.driver.set_page_load_timeout(60)

    def getData(self, url, button, dataTable):
        self.driver.get(url)
        self.driver.find_element_by_css_selector(button).click()

        try:
            element = WebDriverWait(self.driver, 15).until(
            EC.visibility_of_element_located((By.ID, "484656454-pane-2"))
            )

        except:
            warnings.warning("Element never became visible, returning NA")
            return("NA")

        finally:
            self.driver.quit()

        return(element.text)

def scrape_button(AutoURL):
    Session = AutoTraderSession()
    CSSSelectorButton = ".active+ li a , .active+ li span" # This is the model info button
    CSSDataTable = '//*[contains(concat( " ", @class, " " ), concat( " ", "padding-horizontal-lg", " " ))]'

    data = Session.getData(url = AutoURL, button = CSSSelectorButton, dataTable = CSSDataTable)
    Session.driver.quit()
    return data

if __name__ == '__main__':
    scrape_button(URL)
