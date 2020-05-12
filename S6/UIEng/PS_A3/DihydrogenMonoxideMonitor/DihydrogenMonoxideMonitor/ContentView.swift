//
//  ContentView.swift
//  DihydrogenMonoxideMonitor
//
//  Created by ariez on 30.04.20.
//  Code inspired by https://medium.com/flawless-app-stories/fast-app-prototyping-with-swiftui-39ae03ab3eaa

import SwiftUI

struct Wave: Shape {
    let graphWidth: CGFloat
    let amplitude: CGFloat
    let offset: CGFloat
    
    func path(in rect: CGRect) -> Path {
        let width = rect.width * 1.3
        let height = rect.height

        let origin = CGPoint(x: 0, y: height * 0.50)

        var path = Path()
        path.move(to: origin)

        var endY: CGFloat = 0.0
        let step = 5.0
        for pos in stride(from: 0, through: Double(width) * (step * step), by: step) {
            let x = origin.x + CGFloat((pos + Double(offset))/360.0) * width * graphWidth
            let y = origin.y - CGFloat(sin((pos + Double(offset))/180.0 * Double.pi)) * height * amplitude
            path.addLine(to: CGPoint(x: x, y: y))
            endY = y
        }
        path.addLine(to: CGPoint(x: width, y: endY))
        path.addLine(to: CGPoint(x: width, y: height))
        path.addLine(to: CGPoint(x: 0, y: height))
        path.addLine(to: CGPoint(x: 0, y: origin.y))

        return path
    }
}

private var repeatingAnimation: Animation {
    Animation
        .easeInOut
        .speed(0.1)
        .repeatForever()
}

struct WavesBg: View {
    
    // magic numbers galore
    @State private var backgroundOffsetY1: CGFloat = 75
    //@State private var backgroundOffsetY2: CGFloat = 60
    
    @State private var backgroundOffsetX1: CGFloat = -20
    //@State private var backgroundOffsetX2: CGFloat = 10
    
    @State private var foregroundOffsetY1: CGFloat = 65
    //@State private var foregroundOffsetY2: CGFloat = 75
    
    @State private var foregroundOffsetX1: CGFloat = -90
    //@State private var foregroundOffsetX2: CGFloat = -115
    
    @State var offs: CGFloat
    let parent: ContentView
    
    var body: some View {
        return ZStack {
                Wave(graphWidth: 1, amplitude: 0.012, offset: 0)
                    .foregroundColor(Color(red: 0, green: 0.68, blue: 0.85))
                    .offset(x: backgroundOffsetX1, y: backgroundOffsetY1-self.offs)
                    .onAppear {
                        self.backgroundOffsetY1 = 60-self.offs// - self.parent.drank
                        self.backgroundOffsetX1 = 10
                    }.frame(width: UIScreen.main.bounds.size.width, height: 2000)
                    
                Wave(graphWidth: 1, amplitude: 0.017, offset: 0)
                    .foregroundColor(Color(red: 0, green: 0.8, blue: 1))
                    .offset(x: foregroundOffsetX1, y: foregroundOffsetY1-self.offs)
                    .onAppear {
                        self.foregroundOffsetY1 = 75-self.offs// - self.parent.drank
                        self.foregroundOffsetX1 = -115
                }.frame(width: UIScreen.main.bounds.size.width, height: 2000)
                
            }.animation(repeatingAnimation)
        }
}

struct DrinkButton: View {
    var text: String
    var action: () -> Void
    var body: some View {
        Button(action: {
            withAnimation { self.action() }
        }) {
            HStack(spacing: 20) {
                Image(systemName: "gear")
                .resizable()
                .frame(width: 30, height: 30)
                
                Divider()
                    .accentColor(Color.black.opacity(1))
                
                Text(text)
                .font(.system(size: 30, design: .rounded))
            }
            .accentColor(Color.black.opacity(0.8))
        }
        .frame(width: 260, height: 70)
        .background(Color.black.opacity(0.2))
        .cornerRadius(420.69)
    }
}

struct DetailView: View {
    @Environment(\.presentationMode) var presentationMode: Binding<PresentationMode>
    var body: some View {
        Button(
            "Here is Detail View. Tap to go back.",
            action: { self.presentationMode.wrappedValue.dismiss() }
        )
    }
}

struct ContentView: View {
    @State public var drank: CGFloat = 750
    @State private var showAccel: Bool = false
    var step: CGFloat = 360
    var mult:Int=1
    
    var body: some View {
        return ZStack {
            WavesBg(offs: 0, parent: self)
                .position(x: 250, y: UIScreen.main.bounds.size.height - 350)
                .animation(repeatingAnimation)
            
            //Rectangle()
            //    .frame(width: UIScreen.main.bounds.size.width, height: 2*drank)
            //    .foregroundColor(Color(red: 0, green: 0.8, blue: 1))
            //    .position(x: UIScreen.main.bounds.size.width/2, y: UIScreen.main.bounds.size.height)
            
            if self.showAccel {
                Button(action: {
                    withAnimation { self.showAccel = !self.showAccel }
                    
                }) {
                    VStack {
                        HStack {
                            Image(systemName: "skew")
                            .resizable()
                            .frame(width: 30, height: 30)
                            .foregroundColor(.black)

                            Text("AR measurement")
                                .font(.system(size: 20, weight: .bold))
                                .foregroundColor(.black)
                        }
                        
                        Text("Place your device on the table next to your glass of water. Slowly raise the device up to the water level.")
                            .foregroundColor(.black)
                            .padding()
                            .font(.system(size: 18, weight: .semibold))
                        
                        Text("DihydrogenMonoxideMonitor is taking precise measurements using your iPhone's AR depth sensors and accelerometer.")
                        .foregroundColor(.black)
                        .padding()
                        
                        Text("Current reading:")
                        .foregroundColor(.black)
                        
                        Text("360ml")
                        .font(.system(size: 30, design: .rounded))
                        .foregroundColor(.black)
                        
                        Divider()
                        
                        Text("Glass type")
                        
                        
                        HStack {
                            Image(systemName: "circle")
                            .resizable()
                            .frame(width: 15, height: 15)

                            Divider()
                            
                            Image(systemName: "circle")
                            .resizable()
                            .frame(width: 22, height: 22)
                                .foregroundColor(.white)

                            Divider()
                            
                            Image(systemName: "circle")
                            .resizable()
                            .frame(width: 30, height: 30)
                        }
                        .frame(width: 120, height: 40)
                        .background(Color.black.opacity(0.2))
                        .cornerRadius(10)
                    }
                    .foregroundColor(.black)
                }
                .frame(width: UIScreen.main.bounds.size.width-40, height: 420)
                .background(Color.black.opacity(0.2))
                .cornerRadius(25)
                
            } else {
                VStack {
                    DrinkButton(text: "Add " + String(Int(step)*mult) + "ml") {
                        self.showAccel = !self.showAccel
                    }
                    
                    Text(String(self.mult*Int(self.drank)) + "/2000ml")
                }
            }
        }
        .frame(minWidth: .zero, maxWidth: .infinity, minHeight: .zero, maxHeight: .infinity)
        .edgesIgnoringSafeArea(.all)
    }
}

struct ContentView_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
