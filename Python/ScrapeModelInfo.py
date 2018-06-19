# install chromedriver from https://chromedriver.storage.googleapis.com/index.html?path=2.38/
# HANDY LINK: http://isaacviel.name/make-web-driver-wait-element-become-visiable/
# install chromedriver on ubuntu https://tecadmin.net/setup-selenium-chromedriver-on-ubuntu/

import warnings
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC

## For testing (obvi, lmao)
#testURL = "https://www.autotrader.com/cars-for-sale/vehicledetails.xhtml?listingId=483451570&zip=96801&referrer=%2Fcars-for-sale%2Fsearchresults.xhtml%3Fzip%3D96801%26sellerTypes%3Dp%26startYear%3D1981%26numRecords%3D100%26sortBy%3Drelevance%26incremental%3Dall%26firstRecord%3D800%26endYear%3D2019%26modelCodeList%3DWRANGLER%26makeCodeList%3DJEEP%26searchRadius%3D0&sellerTypes=p&startYear=1981&numRecords=100&firstRecord=800&endYear=2019&modelCodeList=WRANGLER&makeCodeList=JEEP&searchRadius=0&makeCode1=JEEP&modelCode1=WRANGLER"

class AutoTraderSession():
    def __init__(self):
        chrome_options = webdriver.ChromeOptions()
        #chrome_options.add_argument('--headless')
        #chrome_options.add_argument('no-sandbox')

        self.browser = webdriver.Chrome(chrome_options= chrome_options)
        self.browser.set_page_load_timeout(60)

    def getData(self, url, button, dataTable):
        self.browser.get(url)
        self.browser.find_element_by_css_selector(button).click()
        print("Clicked")

        try:
            WebDriverWait(self.browser, 15).until(
            EC.visibility_of_element_located((By.XPATH, "//div[@data-qaid='cntnr-md-disclaimer']"))
            )
            print("Seen")

        except:
            warnings.warning("Element never seen. Returning NA")
            return("NA")

        return(self.browser.page_source)
        
    
    def open_url(self, url):
      # try: 
      self.browser.get(url)
        
      # except:
      #   print("lmao")

def scrape_button(AutoURL):
    Session = AutoTraderSession()
    CSSSelectorButton = ".active+ li a , .active+ li span" # This is the model info button
    CSSDataTable = '//*[contains(concat( " ", @class, " " ), concat( " ", "padding-horizontal-lg", " " ))]'

    data = Session.getData(url = AutoURL, button = CSSSelectorButton, dataTable = CSSDataTable)
    Session.browser.quit()
    return data

def visit_url(url):
  Session = AutoTraderSession()
  Session.open_url(url)
  
    
# ## For testing
# if __name__ == '__main__':
  # Session = AutoTraderSession()
  # Session.open_url(testURL)


  
