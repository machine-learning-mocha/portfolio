var gplay = require('google-play-scraper');
const ObjectsToCsv = require('objects-to-csv');

gplay.reviews({
    appId: 'com.bradesco',
    num: 1000
  }).then(formatComment, console.log);

function formatComment(responses) {
    (async () => {
        const csv = new ObjectsToCsv(responses);
       
        // Save to file:
        await csv.toDisk('./com.bradesco.csv');
       
        // Return the CSV file as string:
        console.log(await csv.toString());
      })();
}  