// urlB64ToUint8Array is a magic function that will encode the base64 public key
// to Array buffer which is needed by the subscription option
//const urlB64ToUint8Array = (base64String) => {
//  const padding = '='.repeat((4 - (base64String.length % 4)) % 4)
//  const base64 = (base64String + padding).replace(/\-/g, '+').replace(/_/g, '/')
//  const rawData = atob(base64)
//  const outputArray = new Uint8Array(rawData.length)
//  for (let i = 0; i < rawData.length; ++i) {
//    outputArray[i] = rawData.charCodeAt(i)
//  }
//  return outputArray
//}

//// saveSubscription saves the subscription to the backend
//const saveSubscription = async (subscription) => {
//  const SERVER_URL = '/app/notif/sub'
//  const response = await fetch(SERVER_URL, {
//    method: 'post',
//    headers: {
//      'Content-Type': 'application/json',
//    },
//    body: JSON.stringify(subscription),
//  })
//  return response
//}


self.addEventListener('install', async () => {
//  self.skipWaiting()
  console.log('notif service worker installed')
})

self.addEventListener('activate', async () => {
  // This will be called only once when the service worker is activated.
  console.log('notif service worker activated')
//  try {
//    const applicationServerKey = urlB64ToUint8Array(
//      'BLw7yyZN9FCSJJStBrKoLCxjX8D1DyPdQmMwIIKH8x47TaVLqqYBGlr8rpqRxme762alU3c-ojHug8tFki61f5E='
//    )
//    const options = { applicationServerKey, userVisibleOnly: true }
//    console.log("opts", options)
//    const subscription = await self.registration.pushManager.subscribe(options)
//    console.log("SUB", subscription)
//    const response = await saveSubscription(subscription)
//    console.log(response)
//  } catch (err) {
//    console.log('Error', err)
//  }
})

// Register event listener for the 'push' event.
self.addEventListener('push', function(event) {
  // Retrieve the textual payload from event.data (a PushMessageData object).
  // Other formats are supported (ArrayBuffer, Blob, JSON), check out the documentation
  // on https://developer.mozilla.org/en-US/docs/Web/API/PushMessageData.
  const {title, ...payload} = event.data && event.data.json();

  // Keep the service worker alive until the notification is created.
  event.waitUntil(
    // Show a notification with title 'ServiceWorker Cookbook' and use the payload
    // as the body.
    self.registration.showNotification(title, payload)
  );
});

self.addEventListener("notificationclick", (event) => {
  console.log("On notification click: ", event);
  if (clients.openWindow) {
    let route = event.notification.data.routeTo;
    return clients.openWindow(route || "/");
  }
  event.notification.close();
});