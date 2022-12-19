const { By, Key, until, Builder } = require('selenium-webdriver')
require('chromedriver')
const assert = require('assert')
const { buildDriver, goToHome, captureScreensFor, teardown, delay } = require('../../utils')

const SSOHomePage = require('../../pageobjects/SSOHomePage')
const UAALoginPage = require('../../pageobjects/UAALoginPage')
const OverviewPage = require('../../pageobjects/OverviewPage')

<<<<<<< HEAD
<<<<<<< HEAD
describe("Once user is logged in", function() {
  var homePage;
  var uaaLogin;
  var overview;
  this.timeout(25000); // hard-coded to 25secs because this test requires 25sec to run
=======
describe.skip('Once user is logged in', function () {
=======
describe('Once user is logged in', function () {
>>>>>>> 6e84444059 (Test token refresh)
  let homePage
  let uaaLogin
  let overview
  let captureScreen
  this.timeout(25000) // hard-coded to 25secs because this test requires 25sec to run
>>>>>>> 9354397cbf (Support Idp initiated logon in mgt ui with Oauth)

  before(async function () {
    driver = buildDriver()
    await goToHome(driver)
    homePage = new SSOHomePage(driver)
    uaaLogin = new UAALoginPage(driver)
    overview = new OverviewPage(driver)
    captureScreen = captureScreensFor(driver, __filename)
  })

  it('its token is automatically renewed', async function () {
    await homePage.clickToLogin()
    await uaaLogin.login('rabbit_admin', 'rabbit_admin')
    await overview.isLoaded()

    await delay(15000)
    await overview.isLoaded() // still after accessTokenValiditySeconds = 15 sec
    await overview.clickOnConnectionsTab() // and we can still interact with the ui
  })

  after(async function () {
    await teardown(driver, this, captureScreen)
  })
})
