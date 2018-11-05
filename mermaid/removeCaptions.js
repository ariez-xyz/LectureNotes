var captions = document.getElementsByClassName('mkcaption'); 
for (i = 0; i < captions.length; i++) { 
    captions[i].parentNode.removeChild(captions[i]);
}
