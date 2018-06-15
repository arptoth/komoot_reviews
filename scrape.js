var store = require('app-store-scraper');


for(var i = 1; i < 11;i++){
store.reviews({
  appId: 'de.komoot.berlinbikeapp',
  country: 'us',
  sort: store.sort.HELPFUL,
  page: i
})
.then(console.log)
.catch(console.log);
}

//store.app({id: 447374873}).then(console.log).catch(console.log);
