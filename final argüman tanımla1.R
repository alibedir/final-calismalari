# Final ödevi
# bir argüman tanımlıyoruz, 
# bu argüman belirediğimiz işlemler ile çalışacak.

# İlk aşamada Bir otomobil class'ı oluşturuyoruz.
# Otomobiller ad, silindir hacmi , beygir gücü , maksimum tork , yakıt deposu değerlerine sahip olacak.

otomobil <- setRefClass(Class = "otomobil", fields = c("ad", "silindir", "beygir", "tork", "yakıt"), methods = list(
  initialize = function(ad, silindir, beygir, tork, yakıt){
    .self$ad <- ad
    .self$silindir <- silindir
    .self$beygir <- beygir
    .self$tork <- tork
    .self$yakıt <- yakıt
    },
  
  # otomobil özelliklerini göstereceğimiz fonksiyonlar.
  show_ad = function(){
    return(ad)
  },
  show_silindir = function(){
    return(silindir)
  },
  show_beygir = function(){
    return(beygir)
  },
  show_tork = function(){
    return(tork)
  },
  show_yakıt = function(){
    return(yakıt)
  },
  show_güç= function(){
    güç <- silindir + beygir + tork 
    return(güç)
  },
  show = function(){
    cat("otomobil: ", ad, "\n",
        "silindir:", silindir, "|",
        "beygir:", beygir, "|",
        "tork:", tork, "|",
        "yakıt:", yakıt, "|",
        "güç:", show_güç() )
  },
  # otomobil kıyası
  otomobil_vs = function(otomobil_2){
    cat(" otomobil: ", ad, "|",
        "silindir:", silindir, "|",
        "güç:", show_güç(), "\n",
        "otomobil: ", otomobil_2$ad,"|",
        "silindir:", warrior_2$silindir, "|",
        "güç:", otomobil_2$show_güç()
    )
  }
))


# Bu karşılaştırma için argüman Tanımlama.

'%?%' <- function(x, ...){
  UseMethod("güçlerfarkı", x)
}    
güçlerfarkı.otomobil <- function(a, b){
  list(
    silindir = a$silindir - b$silindir,
    beygir = a$beygir + b$beygir,
    tork = a$tork + b$tork,
      güç = a$show_güç() - b$show_güç()
  )
}

'%!%' <- function(x, ...){
  UseMethod("güçoranı", x)
}    
güçoranı.otomobil <- function(a, b){
  list(
    a_silindir = a$silindir / b$show_güç(),
    b_beygir = b$silindir - a$show_güç()
  )
}


a3 <- otomobil$new("a3", 999, 116, 200, 5.3)
a4 <- otomobil$new("a4", 1968, 190, 400, 4.3)
a3 %?% a4
a3 %!% a4
