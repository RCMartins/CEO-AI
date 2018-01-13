package ceo.ui

import java.awt.event._
import java.awt._
import javax.swing._

import ceo.image.ImageLoader

import scala.util.{Failure, Success}

class PlayUI() extends JFrame {

  var readyToUpdate = false
  var component: GameComponent = _

  SwingUtilities.invokeLater(() => {
    createAndShowGUI()
    setVisible(true)

    component.recalculateSizes()
  })

  new Thread(new Runnable {
    val rand = new scala.util.Random
    val size = 2

    override def run(): Unit = {
      while (true) {
        if (readyToUpdate) {
          component.repaint()
        }
        Thread.sleep(200)
      }
    }
  }).start()

  final val GAME_TITLE = "Chess Evolved Online - Bot"
  final val DEFAULT_WIDTH = 500
  final val DEFAULT_HEIGHT = 800

  def createAndShowGUI() {
    this.setSize(DEFAULT_WIDTH, DEFAULT_HEIGHT)
    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    setLocationRelativeTo(null)

    setTitle(GAME_TITLE)

    val mainPane = new JPanel()
    mainPane.setLayout(new BorderLayout());
    {
      val pane = new JPanel()
      pane.setLayout(new BorderLayout())
      component = new GameComponent(this)
      component.addComponentListener(component)
      component.setFocusable(true)
      pane.add(component, BorderLayout.CENTER)
      pane.setBorder(BorderFactory.createLineBorder(Color.BLACK))
      mainPane.add(pane, BorderLayout.CENTER)
    }

    {
      val buttonsPane = new JPanel()
      buttonsPane.setLayout(new BoxLayout(buttonsPane, BoxLayout.PAGE_AXIS))

      {
        val button = new JButton("Correct!")
        button.setBackground(Color.decode("#32CD32"))
        button.addActionListener((_: ActionEvent) => {
          component.requestFocusInWindow()
          component.repaint()
        })
        buttonsPane.add(button)
      }
      buttonsPane.add(Box.createRigidArea(new Dimension(0, 50)))

      {
        val button = new JButton("Delete!")
        button.setBackground(Color.decode("#FF6347"))
        button.addActionListener((_: ActionEvent) => {
          component.requestFocusInWindow()
          component.repaint()
        })
        buttonsPane.add(button)
      }
      buttonsPane.add(Box.createRigidArea(new Dimension(0, 200)))

      {
        val button = new JButton("Write name")
        button.addActionListener((_: ActionEvent) => {
          Option(JOptionPane.showInputDialog(this, "Write piece name")) match {
            case Some(pieceNameFull) if pieceNameFull.nonEmpty =>
              val (image, inWhiteSquare) = ImageLoader.imagesUnknown.head
              ImageLoader.getPieceFromName(pieceNameFull, inWhiteSquare) match {
                case Success(Some(_)) =>
                  JOptionPane.showMessageDialog(this, "This piece already exists!")
                case Success(None) =>
                  ImageLoader.addNewPieceName(image, pieceNameFull, inWhiteSquare)
                  ImageLoader.imagesUnknown.dequeue()
                case Failure(e) => JOptionPane.showMessageDialog(this, e.getMessage)
              }
              component.requestFocusInWindow()
              component.repaint()
            case None => // ignore
          }
        })
        buttonsPane.add(button)
      }

      {
        val button = new JButton("Skip piece!")
        button.addActionListener((_: ActionEvent) => {
          val first = ImageLoader.imagesUnknown.dequeue()
          ImageLoader.imagesUnknown.enqueue(first)
          component.requestFocusInWindow()
          component.repaint()
        })
        buttonsPane.add(button)
      }
      mainPane.add(buttonsPane, BorderLayout.EAST)
    }

    setContentPane(mainPane)

    component.requestFocusInWindow()

    readyToUpdate = true
  }
}

class GameComponent(val playUI: PlayUI) extends JComponent with ComponentListener {

  override def paintComponent(g: Graphics) {
    val g2: Graphics2D = g.asInstanceOf[Graphics2D]

    var x = 20
    var y = 0
    ImageLoader.imagesToConfirm.foreach { case (pieceImage, pieceData) =>
      g2.drawImage(pieceImage.bufferedImage, x, y, null)
      g2.drawString(pieceData.name, x, y + 70)
      x += 100
    }

    x = 20
    y = 400
    ImageLoader.imagesUnknown.foreach { case (pieceImage, _) =>
      g2.drawImage(pieceImage.bufferedImage, x, y, null)
      y += 100
    }

    // Check all images:
    //    x = 0
    //    y = 0
    //    ImageLoader.allPieceImages.toList.sortBy(_._2._1).map(_._1).foreach { pieceImage =>
    //      g2.drawImage(pieceImage.bufferedImage, x, y, null)
    //      if (x > 1400) {
    //        x = 0
    //        y += 60
    //      } else {
    //        x += 60
    //      }
    //    }
  }

  def componentHidden(e: ComponentEvent) {
  }

  def componentMoved(e: ComponentEvent) {
  }

  def componentResized(e: ComponentEvent) {
    recalculateSizes()
  }

  def componentShown(e: ComponentEvent) {
  }

  def recalculateSizes() {
    repaint()
  }
}
