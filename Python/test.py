# install chromedriver from https://chromedriver.storage.googleapis.com/index.html?path=2.38/
# HANDY LINK: http://isaacviel.name/make-web-driver-wait-element-become-visiable/
# install chromedriver on ubuntu https://tecadmin.net/setup-selenium-chromedriver-on-ubuntu/

import warnings
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC

## For testing (obvi, lmao)
testURL = "https://www.overstock.com/?prop45=o.co"

class AutoTraderSession():
    def __init__(self):
        chrome_options = webdriver.ChromeOptions()
        #chrome_options.add_argument('--headless')
        #chrome_options.add_argument('no-sandbox')

        self.browser = webdriver.Chrome(chrome_options= chrome_options)
        self.browser.set_page_load_timeout(30)

    def getData(self, url, button, dataTable):
        self.browser.get(url)
        self.browser.find_element_by_css_selector(button).click()
        print("Clicked")
        print(self.browser.get_cookies())

        try:
            WebDriverWait(self.browser, 15).until(
            EC.visibility_of_element_located((By.XPATH, '//*[contains(concat( " ", @class, " " ), concat( " ", "heading-2", " " ))]'))
            )
            print("Seen")

        except:
            #warnings.warning("Element never seen. Returning NA")
            return("NA")

        self.browser.find_element_by_css_selector(".button").click()


        return(self.browser.page_source)

def scrape_button(AutoURL):
    Session = AutoTraderSession()
    CSSSelectorButton = ".top:nth-child(11) .top-nav-links-line" # This is the model info button
    CSSDataTable = '//*[contains(concat( " ", @class, " " ), concat( " ", "heading-2", " " ))]'

    data = Session.getData(url = AutoURL, button = CSSSelectorButton, dataTable = CSSDataTable)
    Session.browser.quit()

    print("\n\n\n\n\n WOW 😱 \n\n\n\n\n")

    return data

## For testing
if __name__ == '__main__':
     scrape_button(testURL)
