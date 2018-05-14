# install chromedriver from https://chromedriver.storage.googleapis.com/index.html?path=2.38/
from selenium import webdriver

class AutoTraderSession():
    def __init__(self):
        self.browser = webdriver.Chrome(r'chromedriver')

    def getData(self, url, CSSselector):
        self.browser.get(url)
        self.browser.find_element_by_css_selector(CSSselector).click()
        return self.browser.page_source

def main(AutoURL):
    Session = AutoTraderSession()
    CSSSelectorButton = ".active+ li a , .active+ li span" # This is the model info button

    return Session.getData(url = AutoURL, CSSselector = CSSSelectorButton)
    Session.browser.quit()

    
