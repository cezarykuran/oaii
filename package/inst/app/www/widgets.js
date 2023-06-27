oaii = {
  scrollDown : function(id) {
    obj = document.getElementById(id)
    obj.scrollTop = obj.scrollHeight
  },

  textConsole : {
    attachEvent : function(id, inputId) {
      $('#' + inputId)
        .data("textConsoleId", id)
        .on("keydown", oaii.textConsole.onkeydown)
    },
    onkeydown : function(e) {
      e = e || window.event;
      if (e.keyCode == 13 && !e.shiftKey) {
        e.preventDefault()
        $obj = $(e.target)
        Shiny.setInputValue($obj.data("textConsoleId"), $obj.val(), {priority: 'event'})
      }
    },
    messageHandler : function(message) {
      obj = document.getElementById(message.inputId)
      switch (message.command) {
        case 'enable':
          obj.disabled = false;
          obj.focus();
          break;
        case 'disable':
          obj.disabled = true;
          break;
        case 'reset':
          obj.value = "";
          break;
        default:
          console.log("unknown message ", message)
      }
    }
  },

  images : {
    container : {
      fsInOut : function(obj) {
        $obj = $(obj)
        if($obj.hasClass("oaii-imagesSetImageContainerFS")) {
          $obj.removeClass("oaii-imagesSetImageContainerFS")
          $obj.parent().css("height", "")
        }
        else {
          $obj.parent().css("height", $obj.parent().height())
          $obj.addClass("oaii-imagesSetImageContainerFS")
        }
      },
      download : function(obj, e, filename = "image.png") {
        e = event || window.event
        e.stopPropagation()

        let a = document.createElement('a');
        a.href = $(obj).parent().children("img").attr("src")
        a.download = filename
        a.click()
        return false
      }
    },

    edit : {
      $container : null,
      canvasFile : null,
      ctxFile : null,
      ctxDraw : null,
      imgFileId : null,
      imgFileW : null,
      imgFileH: null,
      colorDraw : null,

      init : function(idContainer, idImg) {
        oaii.images.edit.$container = $(document.getElementById(idContainer))
        oaii.images.edit.imgFileId = idImg

        $canvas = oaii.images.edit.$container.children("canvas")
        oaii.images.edit.canvasFile = $canvas.get(0)
        oaii.images.edit.ctxFile = $canvas.get(0).getContext('2d')
        oaii.images.edit.ctxDraw = $canvas.get(1).getContext('2d')

        Shiny.addCustomMessageHandler("oaii.images.edit", oaii.images.edit.messageHandler)
        $(window).on("resize", oaii.images.edit.resize)
        oaii.images.edit.$container
          .on('mousedown', oaii.images.edit.start)
          .on('mouseup', oaii.images.edit.stop)
      },

      messageHandler : function(message) {
        switch (message.cmd) {
          case 'resize':
            oaii.images.edit.resize()
            break

          case 'file':
            var image = new Image();
            image.onload = function() {
              oaii.images.edit.imgFileW = image.width
              oaii.images.edit.imgFileH = image.height

              oaii.images.edit.ctxFile.reset()
              oaii.images.edit.$container.children("canvas")
                .prop("width", image.width)
                .prop("height", image.height)
              oaii.images.edit.ctxFile.fillStyle = "#fff"
              oaii.images.edit.ctxFile.drawImage(image, 0, 0)
              oaii.images.edit.ctxFile.globalCompositeOperation = "destination-out"
            }
            image.src = "data:image/png;base64," + message.data
            break

          case 'colorBg':
            oaii.images.edit.$container.css("background-color", message.data)
            break

          case 'colorDraw':
            oaii.images.edit.colorDraw = message.data
            break

          default:
            console.log("oaii.images.edit.messageHandler unknown message: " + message)
        }
      },

      resize : function() {
        $container = oaii.images.edit.$container
        $container
          .height($container.width())
          .children("canvas")
            .width($container.innerWidth())
            .height($container.innerHeight())
      },

      ctxXY : function(e) {
        var offset = oaii.images.edit.$container.offset()
        var scaleX = oaii.images.edit.imgFileW / oaii.images.edit.$container.innerWidth()
        var scaleY = oaii.images.edit.imgFileH / oaii.images.edit.$container.innerHeight()
        return {
          x: (e.clientX - offset.left + window.scrollX) * scaleX,
          y: (e.clientY - offset.top + window.scrollY) * scaleY
        }
      },

      move : function(e) {
        p = oaii.images.edit.ctxXY(e)
        oaii.images.edit.ctxDraw.fillRect(p.x, p.y, 2, 2)
        oaii.images.edit.ctxFile.lineTo(p.x, p.y)
      },

      start : function(e) {
        p = oaii.images.edit.ctxXY(e)
        oaii.images.edit.ctxDraw.fillStyle = oaii.images.edit.colorDraw;
        oaii.images.edit.ctxFile.beginPath()
        oaii.images.edit.ctxFile.moveTo(p.x, p.y)
        oaii.images.edit.$container.on('mousemove', oaii.images.edit.move);
      },

      stop : function(e) {
        oaii.images.edit.$container.off('mousemove', oaii.images.edit.move);
        oaii.images.edit.ctxFile.closePath()
        oaii.images.edit.ctxFile.fill()
        oaii.images.edit.ctxDraw.reset()

        Shiny.setInputValue(
          oaii.images.edit.imgFileId,
          oaii.images.edit.canvasFile.toDataURL("image/png")
        )
      }
    }
  },

  tableBtn : function(n, v) {
    Shiny.setInputValue(n, v)
  }
}
Shiny.addCustomMessageHandler("oaii.textConsole", oaii.textConsole.messageHandler)
